library(readr)
library(dplyr)
library(lubridate)
library(sf)

# this script should run on by the .pos file. 

# --------------------- 

# INPUTS

# directory with photos that belong to the ENTIRE pos file: 
photos_dir <- 'F:\\Bohn2025_1\\EaglePod\\20250609\\SonyILXLR1'

# location of the time offset summary
time_offset <- "F:\\Bohn2025_1\\EaglePod\\20250609\\Geotagged\\time_offset_summary.csv"

# POS file and parse timestamps
pos_file <- "F:\\Bohn2025_1\\EaglePod\\20250609\\events_pos_CACS\\events_pos\\ReconMini_raw_20250609172043.txt"


# ----------------------

# PROCESSING

pos_data <- read.table(pos_file, 
                          header = TRUE,
                          sep = "", 
                          stringsAsFactors = FALSE) %>% 
  mutate(gpst_string = paste(X., GPST)) %>% # Combine date and time strings
  mutate(datetime_utc = strptime(gpst_string, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC") )%>%
  as_tibble()

pos_data$gpst_string %>% head(10)
pos_data$datetime_utc %>% head(10)

# STEP 2: Identify time gaps > 1 second
pos_data <- pos_data %>%
  arrange(datetime_utc) %>%
  mutate(
    time_diff = as.numeric(difftime(lead(datetime_utc), datetime_utc, units = "secs"))
  )

pos_data$time_diff %>% head()

gap_threshold <- 1  # seconds
gap_indices <- which(pos_data$time_diff > gap_threshold)

print(paste("Found", length(gap_indices), "gaps"))

# STEP 3: Interpolate pseudo-points within each gap
interpolated_points <- list()

library(tibble)
library(geosphere)

interpolated_points <- list()

for (i in gap_indices) {
  t1 <- pos_data$datetime_utc[i]
  t2 <- pos_data$datetime_utc[i + 1]
  
  lat1 <- pos_data$latitude.deg.[i]
  lat2 <- pos_data$latitude.deg.[i + 1]
  
  lon1 <- pos_data$longitude.deg.[i]
  lon2 <- pos_data$longitude.deg.[i + 1]
  
  h1 <- pos_data$height.m.[i]
  h2 <- pos_data$height.m.[i + 1]
  
  # Local window around the gap
  window_size <- 5
  start_idx <- max(i - window_size, 1)
  end_idx <- min(i + 1 + window_size, nrow(pos_data))
  
  # Compute distances between successive points in local window
  local_coords <- pos_data[start_idx:end_idx, c("longitude.deg.", "latitude.deg.")]
  dists <- geosphere::distGeo(local_coords[-nrow(local_coords), ], local_coords[-1, ])
  avg_spacing <- median(dists, na.rm = TRUE)  # in meters
  
  # Total gap distance
  gap_distance <- geosphere::distGeo(c(lon1, lat1), c(lon2, lat2))
  
  # Estimate number of interpolated points
  n_points <- floor(gap_distance / avg_spacing) - 1
  
  if (n_points > 0) {
    # Fractional position along line
    frac <- seq(0, 1, length.out = n_points + 2)[-c(1, n_points + 2)]
    
    # Base date (from column X.)
    date_str <- pos_data$X.[i]
    
    # Start GPST time
    gpst_start <- strptime(pos_data$GPST[i], format = "%H:%M:%OS", tz = "UTC")
    gpst_posix <- gpst_start + seq(0.1, 0.1 * n_points, by = 0.1)
    gpst_str <- format(gpst_posix, "%H:%M:%OS3")
    gpst_full <- paste(date_str, gpst_str)
    datetime_vals <- strptime(gpst_full, format = "%Y/%m/%d %H:%M:%OS", tz = "UTC")
    
    # Build tibble
    interp <- tibble(
      X. = rep(date_str, n_points),
      GPST = gpst_str,
      gpst_string = gpst_full,
      datetime_utc = datetime_vals,
      latitude.deg. = lat1 + frac * (lat2 - lat1),
      longitude.deg. = lon1 + frac * (lon2 - lon1),
      height.m. = round(h1 + frac * (h2 - h1), 4),
      Q = NA, ns = NA, sdn = NA, sde = NA, sdu = NA,
      sdne = NA, sdeu = NA, sdun = NA, age = NA, ratio = NA,
      is_pseudo = TRUE
    )
    
    interpolated_points[[length(interpolated_points) + 1]] <- interp
  }
}


interpolated_points

# STEP 4: Combine with original points
pos_data <- pos_data %>%
  mutate(is_pseudo = FALSE)

all_points <- bind_rows(pos_data, bind_rows(interpolated_points)) %>%
  arrange(datetime_utc)

head(all_points)

all_points$datetime_utc %>% head()

# lets export this 
write.csv(all_points, 
          file = file.path(dirname(time_offset), 'pos_pseudo.csv'), 
          row.names = F)


# STEP 5 (Optional): Convert to sf
all_points_sf <- st_as_sf(all_points, coords = c("longitude.deg.", "latitude.deg."), crs = 4326)

st_write(all_points_sf,
         file.path(dirname(time_offset), 'pos_pseudo.gpkg'))

# ---------------------------- #

# now dealing with the photos. 
library(tidyverse)
library(exifr)

configure_exiftool(command = "C:\\Users\\X\\Documents\\exiftool-13.24_64\\exiftool_files\\exiftool.pl",
                   perl_path = "C:\\Users\\X\\Documents\\exiftool-13.24_64\\exiftool_files\\perl.exe")


# 1. List all JPG files (recursive)
photos_files <- list.files(photos_dir, pattern = "\\.JPG$", recursive = TRUE, full.names = TRUE)

# 2. Read EXIF metadata
exif_data <- read_exif(photos_files, tags = "SubSecDateTimeOriginal")

# 3. Build tibble
photo_data <- tibble(
  path = exif_data$SourceFile,
  filename = basename(exif_data$SourceFile),
  SubSecDateTimeOriginal = exif_data$SubSecDateTimeOriginal
)

photo_data$SubSecDateTimeOriginal %>% head()

photo_data <- photo_data %>%
  mutate(datetime_utc = ymd_hms(SubSecDateTimeOriginal, truncated = 3)) 

photo_data$datetime_utc %>% head()

# I need to adjust for the offset. 

offset <- read.csv(time_offset) %>% as_tibble() %>% pull(time_offset_seconds)
offset

photo_data <- photo_data %>%
  mutate(datetime_utc_offset = datetime_utc - seconds(offset))

photo_data$datetime_utc_offset %>% head()
photo_data$datetime_utc %>% head()

all_points$datetime_utc %>% head()

# also export the photo data
write.csv(photo_data, 
          file = file.path(dirname(time_offset), 'photos.csv'), 
          row.names = F)

# ---------

# finding matches

library(dplyr)
library(purrr)

matched <- photo_data %>%
  mutate(closest_idx = map_int(datetime_utc_offset, ~ which.min(abs(difftime(all_points$datetime_utc, .x, units = "secs"))))) %>%
  bind_cols(all_points[.$closest_idx, ])

matched

matched <- matched %>% 
  rename(
    datetime_utc_photo = `datetime_utc...4`,
    datetime_utc_pos   = `datetime_utc...23`,
    datetime_utc_photo_offset = datetime_utc_offset
  )

matched
# I think it looks alright? 

# lets export this. 
write.csv(matched, 
          file = file.path(dirname(time_offset), 'photos_pos_matches.csv'), 
          row.names = F)

# ---------

# ok, lets move to the next days. 
# and then all the next days are done, 




