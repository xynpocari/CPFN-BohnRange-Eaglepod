library(dplyr)
library(readr)
library(stringr)
library(fs)
library(purrr)

# Paths
base_folder <- "\\\\192.168.0.76\\data\\ProjectAreas\\Bohn\\Photos_By_Line_ID_Geotagged"
source_photos <- "\\\\192.168.0.76\\data\\ProjectAreas\\Bohn\\EaglePod\\Day4_20250609\\SonyILXLR1"
lines <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\Bohn_DataCollectionLines.gpkg")

lines_subset <- lines %>% filter(collect_20250609 == 'yes')
lines_subset


# Get all photo files in source (recursive search in all subfolders like 100MSDCF, 101MSDCF)
all_photos <- list.files(source_photos, pattern = "\\.JPG$", full.names = TRUE, recursive = TRUE)

# Subset of line IDs
line_ids <- lines_subset$ID

# Exclude specific IDs
exclude_ids <- c("NS-2", "NS-4" , "NS-5"  ,"NS-6" , "NS-7" , "NS-9")
line_ids <- setdiff(line_ids, exclude_ids)

# Initialize summary list
summary_list <- list()

i <- 2
for (i in 2:length(line_ids)) {
  # Path to the line folder
  line_id <- line_ids[i]
  line_folder <- file.path(base_folder, line_id)
  
  # Path to Agisoft CSV inside that folder
  csv_file <- file.path(line_folder, paste0("coordinates_agisoft_", line_id, ".csv"))
  
  if (!file.exists(csv_file)) {
    cat("⚠ No CSV found for", line_id, "- skipping.\n")
    summary_list[[length(summary_list) + 1]] <- tibble(
      line_id = line_id,
      status = "No CSV",
      photos_copied = 0
    )
    next
  }
  
  # Read CSV and get photo names
  coords_df <- read_csv(csv_file, show_col_types = FALSE)
  
  # filter out ones that arent on the right date
  coords_df <- coords_df %>%
    filter(as_date(datetime_utc_photo_offset) == ymd("2025-06-09"))
  
  photo_names <- coords_df$ID
  
  # Find matching source photos
  matching_photos <- all_photos[basename(all_photos) %in% photo_names]
  
  if (length(matching_photos) == 0) {
    cat("⚠ No matching photos found for", line_id, "\n")
    summary_list[[length(summary_list) + 1]] <- tibble(
      line_id = line_id,
      status = "No matching photos",
      photos_copied = 0
    )
    next
  }
  
  # Destination photos folder (create if not exists)
  photos_folder <- file.path(line_folder, "photos")
  
  # Copy files
  file_copy(matching_photos, photos_folder)
  
  # Log success
  summary_list[[length(summary_list) + 1]] <- tibble(
    line_id = line_id,
    status = "Copied",
    photos_copied = length(matching_photos)
  )
  
  cat("✅ Copied", length(matching_photos), "photos for", line_id, "\n")
}

# Combine summary into a single dataframe
summary_df <- bind_rows(summary_list)

# Save summary as CSV
summary_file <- file.path(base_folder, "photo_copy_summary_20250609.csv")
write_csv(summary_df, summary_file)

cat("📄 Summary saved to:", summary_file, "\n")
