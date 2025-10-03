# previously, I interpolated positions for all the photos based on 
# time and the .pos entries. 

# so now, lets try to update the agisoft coords csvs. 

library(dplyr)
library(stringr)
library(readr)

# --- Directories ---
coords_dir <- "F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\agisoft_coords_utm"
matches_dir <- "F:\\Bohn2025_1\\EaglePod"
output_dir <- "F:\\Bohn2025_1\\Geotagging\\agisoft_coords_NRCAN_PPP_interpolated"  


# 1. Read all matches files (from NRCAN_PPP_pos_photos_matches)
matches_files <- list.files(matches_dir, pattern = "NRCAN_PPP_pos_photos_interpolation\\.csv$", full.names = TRUE, 
                            recursive = TRUE)

matches_files

matches_all <- lapply(matches_files, read_csv) %>%
  bind_rows()

matches_all %>% colnames()

# Ensure matches have line_ID column (it should already exist)
# We'll assume `matches_all` has: filename, Easting, Northing, HGT.m., is_pseudo, line_ID

# 2. Read all Agisoft coordinate files
coords_files <- list.files(coords_dir, pattern = "^coordinates_agisoft_.*\\.csv$", full.names = TRUE)


# Loop through each Agisoft CSV
i <- 1

for (i in 1:length(coords_files)) {
  file <- coords_files[i]
  
  # Extract combined line IDs from filename
  combined_ids <- str_match(basename(file), "coordinates_agisoft_(.*)\\.csv$")[, 2]
  
  # Extract all line IDs from combined name
  line_ids <- str_extract_all(combined_ids, "[A-Za-z]+-\\d+")[[1]]
  
  # Read Agisoft CSV
  agisoft_df <- read_csv(file)
  
  # Filter matches for these line IDs
  matches_line <- matches_all %>%
    filter(line_ID %in% line_ids) %>%
    select(filename, UTM_EASTING, UTM_NORTHING, H.CGVD2013.m., is_pseudo,
           SDLAT_avg, SDLON_avg, SDHGT_avg, datetime_utc_photo_offset, 
           line_ID) 
  
  if (nrow(matches_line) == 0) {
    cat("⚠ No matches found for", combined_ids, "- skipping.\n")
    next
  }
  
  # Join and update
  updated_df <- agisoft_df %>%
    select(-is_pseudo) %>%
    left_join(matches_line, by = c("ID" = "filename")) %>%
    
    # Add pseudo backup columns
    mutate(
      
      # Replace coordinates with PPP values
      Easting = UTM_EASTING,
      Northing = UTM_NORTHING,
      H2013 = H.CGVD2013.m.,
      
      # Replace accuracy cols with SDLON/SDLAT/SDHGT × 2, rounded to 3 decimals
      X_acc = round(SDLON_avg * 2, 3),
      Y_acc = round(SDLAT_avg * 2, 3),
      Z_acc = round(SDHGT_avg * 2, 3)
    ) %>%
    
    # If is_pseudo = TRUE, make accuracy 10 m 
    mutate(
      X_acc = ifelse(is_pseudo, 10, X_acc),
      Y_acc = ifelse(is_pseudo, 10, Y_acc),
      Z_acc = ifelse(is_pseudo, 10, Z_acc)
    ) %>%
    
    # Drop helper columns from matches
    select(-UTM_EASTING, -UTM_NORTHING, -H.CGVD2013.m.)
  
  # Save updated CSV in output folder
  output_file <- file.path(output_dir, paste0("coordinates_agisoft_", combined_ids, ".csv"))
  write_csv(updated_df, output_file)
  
  cat("✅ Updated file saved:", output_file, "\n")
}

