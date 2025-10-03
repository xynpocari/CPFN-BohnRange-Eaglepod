# DONT USE THIS!! ARCHIVE

# here I match the photo timestamp to the nearest .pos entry. 
# but it was decidded that we should interpolate instead!!!!!

# see script labelled as INTERPOLATION

# -----------



# so after looking at the prev results, 
# we decided to use NRCAN PPP service for kinematic positioning instead. 


# so now I need to backtrack a little bit, which is interesting. 


# I have a new .pos file from NRCAN, so I need to take that, and do the interpolation

# then, I need to match the photos to a position by time.

# so... let me see about that now. 

library(sf)
library(dplyr)
library(tibble)
library(tools)
library(lubridate)
library(purrr)

op <- options(digits.secs=6)

# Path to your .pos file from NRCAN PPP
pos_file <- "F:\\Bohn2025_1\\EaglePod\\20250606\\NRCAN_PPP_Kinematic\\ReconMini_raw_20250606171721.pos"

# path to the matched file, made previously. 
matches_file <- "F:\\Bohn2025_1\\EaglePod\\20250606\\Geotagged\\photos_group_filter_lineID_testing.csv"

# location of the time offset summary
time_offset <- "F:\\Bohn2025_1\\EaglePod\\20250606\\Geotagged\\time_offset_summary.csv"

# -------------------------------------- #

# 1. Read all lines
all_lines <- readLines(pos_file)
all_lines %>% head()

# 2. Find the header line (starts with "DIR")
header_line_idx <- grep("^DIR", all_lines)

# 3. Extract header and data
header <- strsplit(all_lines[header_line_idx], "\\s+")[[1]]
data_lines <- all_lines[(header_line_idx + 1):length(all_lines)]
data_lines %>% head()

# 4. Read data into a dataframe
# Combine lines into a text connection so read.table can read them
data_conn <- textConnection(data_lines)
df <- read.table(data_conn, header = FALSE, col.names = header, fill = TRUE)
close(data_conn)

df %>% head()
df %>% colnames()

# 5. Build the output file path
pos_dir <- dirname(pos_file)                              # Folder of input file
pos_name <- file_path_sans_ext(basename(pos_file))        # Filename without extension
output_file <- file.path(pos_dir, paste0(pos_name, "_edited.csv"))

# 5. Write to CSV
write.csv(df, output_file, row.names = FALSE)

cat("CSV saved to:", output_file, "\n")

# ------------------------------------------------- #

# now, I want to remove the entries which have bad standard dev. 

df_filtered <- df %>%
  filter(SDLAT.95.. <= 1, SDLON.95.. <= 1, SDHGT.95.. <= 1)

df_filtered <- df_filtered %>% as_tibble()

# parsing a lubridate datetime
df_filtered <- df_filtered %>%
  mutate(
    datetime_utc = ymd_hms(
      paste(YEAR.MM.DD, HR.MN.SS.SS),
      tz = "UTC",
      truncated = 3
    )
  )

df_filtered$datetime_utc %>% unique() %>% length()

# do the thing where I interpolate. 

# Identify time gaps > 1 second
df_filtered <- df_filtered %>%
  arrange(datetime_utc) %>%
  mutate(
    time_diff = as.numeric(difftime(lead(datetime_utc), datetime_utc, units = "secs"))
  )

df_filtered %>% colnames()

df_filtered$time_diff %>% head()

gap_threshold <- 1  # seconds
gap_indices <- which(df_filtered$time_diff > gap_threshold)

print(paste("Found", length(gap_indices), "gaps"))

# Interpolate pseudo-points within each gap
interpolated_points <- list()

for (i in gap_indices) {
  t1 <- df_filtered$datetime_utc[i]
  t2 <- df_filtered$datetime_utc[i + 1]
  
  # UTM coordinates and height
  x1 <- df_filtered$UTM_EASTING[i]
  x2 <- df_filtered$UTM_EASTING[i + 1]
  
  y1 <- df_filtered$UTM_NORTHING[i]
  y2 <- df_filtered$UTM_NORTHING[i + 1]
  
  h1 <- df_filtered$H.CGVD2013.m.[i]
  h2 <- df_filtered$H.CGVD2013.m.[i + 1]
  
  # Define local window to estimate average spacing
  window_size <- 5
  start_idx <- max(i - window_size, 1)
  end_idx <- min(i + 1 + window_size, nrow(df_filtered))
  
  local_coords <- df_filtered[start_idx:end_idx, c("UTM_EASTING", "UTM_NORTHING")]
  
  # Compute distances between successive points in local window (Euclidean)
  dists <- sqrt(rowSums((local_coords[-nrow(local_coords), ] - local_coords[-1, ])^2))
  avg_spacing <- median(dists, na.rm = TRUE)  # in meters
  
  # Total gap distance
  gap_distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  # Estimate number of interpolated points
  n_points <- floor(gap_distance / avg_spacing) - 1
  
  if (n_points > 0) {
    frac <- seq(0, 1, length.out = n_points + 2)[-c(1, n_points + 2)]
    
    # Interpolated times
    total_gap_time <- as.numeric(difftime(t2, t1, units = "secs"))
    interp_times <- t1 + frac * total_gap_time
    
    # Build tibble of interpolated points
    interp <- tibble(
      datetime_utc = interp_times,
      UTM_EASTING = x1 + frac * (x2 - x1),
      UTM_NORTHING = y1 + frac * (y2 - y1),
      H.CGVD2013.m. = round(h1 + frac * (h2 - h1), 4),
      is_pseudo = TRUE
    )
    
    interpolated_points[[length(interpolated_points) + 1]] <- interp
  }
}

# Combine interpolated points into a single dataframe
if (length(interpolated_points) > 0) {
  interpolated_df <- bind_rows(interpolated_points)
} else {
  interpolated_df <- tibble()
}

interpolated_df %>% head()

# Combine original + interpolated and sort by time
final_df <- bind_rows(
  df_filtered %>%
    mutate(is_pseudo = FALSE),
  interpolated_df
) %>%
  arrange(datetime_utc)


# --- Save result to CSV in same folder ---
input_path <- pos_file
output_file <- file.path(dirname(input_path),
                         paste0(tools::file_path_sans_ext(basename(input_path)), "_interpolated.csv"))
write.csv(final_df, output_file, row.names = FALSE)

cat("Interpolated CSV saved to:", output_file, "\n")

# ----------------------------------------------- #

# that part is good now. now I think I need to match using the 
# existing matches that I made before? 

matches <- read.csv(matches_file) %>% as_tibble()
matches %>% colnames()

# let me remove the cols which are from the previous .pos from emlid studio. 


# Find column positions
start_col <- match("closest_idx", names(matches))
end_col <- match("age", names(matches))

# Get the column names for that range
cols_to_remove <- names(matches)[start_col:end_col]

# Drop by names safely
matches_clean <- matches %>%
  select(-all_of(cols_to_remove))

# Check
colnames(matches_clean)
matches_clean


# okay... now I need to parse the datetime to a lubridate datetime.
# for some reason there's an error in some files, 
# so that offset time is not correct. which is a bummer. 

matches_clean <- matches_clean %>%
  mutate(datetime_utc_photo = ymd_hms(datetime_utc_photo))

# and apply the offset
offset <- read.csv(time_offset) %>% as_tibble() %>% pull(time_offset_seconds)
offset

matches_clean <- matches_clean %>%
  mutate(datetime_utc_photo_offset = (datetime_utc_photo - seconds(offset)))

matches_clean

# --------------- 

# time to find the new matches. 

matches_new <- matches_clean %>%
  mutate(closest_idx = map_int(datetime_utc_photo_offset, ~ which.min(abs(difftime(final_df$datetime_utc, .x, units = "secs"))))) %>%
  bind_cols(final_df[.$closest_idx, ])

matches_new 
matches_new %>% colnames()
matches_new %>% View()

# that seems ok. 

matches_new <- matches_new %>% 
  rename(
    datetime_utc_pos   = datetime_utc
  )

# im going to export this. 
write.csv(matches_new, 
          file = file.path(dirname(pos_file), 'NRCAN_PPP_pos_photos_matches.csv'), 
          row.names = F)


