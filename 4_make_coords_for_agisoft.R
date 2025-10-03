library(dplyr)

output_folder <- 'F:\\Bohn2025_1\\Geotagging\\Lines_by_ID'

matches <- read.csv("F:\\Bohn2025_1\\EaglePod\\20250608\\Geotagged\\photos_group_filter_lineID.csv") %>%
  as_tibble()

matches %>% head()

# For each line_ID
unique_line_ids <- unique(matches$line_ID)

for (line_id in unique_line_ids) {
  
  # Subset by line ID
  line_subset <- matches %>%
    filter(line_ID == line_id) %>%
    select(ID = filename,
           Latitude = latitude.deg.,
           Longitude = longitude.deg.,
           Altitude = height.m.)
  
  # Define output path with renamed CSV
  line_folder <- file.path(output_folder, line_id)
  out_file <- file.path(line_folder, paste0("coordinates_agisoft_", line_id, ".csv"))
  
  # Write CSV
  write_csv(line_subset, out_file)
  
  # Message
  message("Exported: ", out_file)
}
