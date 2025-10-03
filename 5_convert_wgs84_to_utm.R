library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(sf)

# Your list of files
files <- list.files(path = '\\\\192.168.0.76\\data\\ProjectAreas\\Bohn\\Photos_By_Line_ID_Geotagged', 
                    pattern = '\\.csv$', 
                    recursive = TRUE, 
                    full.names = TRUE)

# Read and bind all files with a new column `source_file`
all_data <- map_dfr(files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # Add source file name
  df$source_file <- basename(file)
  
  # Extract line ID (e.g., Short-63, NS-12, EW-4, OB-9, etc.)
  df$line_id <- str_extract(file, "(Short|NS|EW|OB)-[^/\\\\]+")
  
  return(df)
})

all_data


all_data_sf <- st_as_sf(all_data, coords = c('Longitude', 'Latitude'), crs = 4326, remove = FALSE)

# now transform to UTM zone 12

all_data_sf <- st_transform(all_data_sf, crs = 2956)

all_data_sf %>% st_coordinates()

all_data_sf <- all_data_sf %>% 
  mutate(Easting = st_coordinates(all_data_sf)[,1], 
         Northing = st_coordinates(all_data_sf)[,2])

all_data_sf

all_data_sf <- all_data_sf %>%
  mutate(Easting_int = as.integer(Easting*100), 
         Northing_int = as.integer(Northing*100))
  

# ------------------------------- #

# I also need to match up with the other csvs, because I need to know if its real or interpolated. 

match_files <- list.files(path = 'F:\\Bohn2025_1\\EaglePod', 
                          pattern = '_lineID.csv$', 
                          recursive = TRUE, 
                          full.names = TRUE)

match_files <- match_files[1:5]

match_data <- map_dfr(match_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  df$source_file <- basename(file)  # e.g., "photos_group_filter_lineID.csv"
  df$date_folder <- str_extract(file, "\\d{8}")  # e.g., "20250606"
  return(df)
})

match_data
print(match_data[1,])

match_sf <- match_data %>%
  filter(!is.na(latitude.deg.), !is.na(longitude.deg.)) %>%
  st_as_sf(coords = c("longitude.deg.", "latitude.deg."), crs = 4326)

match_sf <- st_transform(match_sf, crs = st_crs(all_data_sf))

match_sf <- match_sf %>% 
  mutate(Easting = st_coordinates(match_sf)[,1], 
         Northing = st_coordinates(match_sf)[,2])

match_sf <- match_sf %>%
  mutate(Easting_int = as.integer(Easting*100), 
         Northing_int = as.integer(Northing*100))

match_sf$Easting_int
# ----------------------------

joined_data <- all_data_sf %>% st_drop_geometry() %>%
  left_join(
    match_sf %>% st_drop_geometry() %>% 
      select(filename, Easting_int, Northing_int, is_pseudo),
    by = c("ID" = "filename", "Easting_int", "Northing_int")
  )

joined_data

table(joined_data$is_pseudo, useNA = "ifany")

joined_data

joined_data <- joined_data %>%
  mutate(
    X_acc = if_else(is_pseudo, 10, 2),
    Y_acc = if_else(is_pseudo, 10, 2),
    Z_acc = if_else(is_pseudo, 20, 4)
  )

# ok, I export this now 

write.csv(joined_data, 
          'F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\0_joined_data.csv', 
          row.names = F)

# and a version for gpsh

joined_data

gpsh_input <- joined_data %>%
  transmute(
    Station = ID,
    Zone = 12,  
    Easting = Easting,
    Northing = Northing,
    h = Altitude
  )

write.csv(gpsh_input, 
          'F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\1_joined_data_gpsh_input.csv', 
          row.names = F)

# ----------------------------------

# okay, now I did all the conversions in GPSH. 

# lets add this back in now. 

gpsh_output <- read.csv("F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\5_joined_data_gpsh_output_conversiongrid_clean.csv") %>%
  as_tibble()

gpsh_output

joined_data$H2013 <- gpsh_output$H2013

# lets export all of this 
write.csv(joined_data, 
          'F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\6_joined_data_H2013.csv', 
          row.names = F)

# and lets make a cleaner version of this. 

joined_data_clean <- joined_data %>% 
  select(ID, Easting, Northing, H2013, X_acc, Y_acc, Z_acc, is_pseudo, source_file)

joined_data_clean

write.csv(joined_data_clean, 
          'F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\7_joined_data_H2013_clean.csv', 
          row.names = F)


joined_data_clean$source_file %>% unique()

# ------------------------

# splitting and saving

# Create an output folder if needed
output_dir <- "F:\\Bohn2025_1\\Geotagging\\convert_coords_to_utm\\agisoft_coords_utm"

# Split by source_file and write each to a CSV
joined_data_clean %>%
  group_split(source_file) %>%
  walk(function(df) {
    file_name <- unique(df$source_file)
    
    # Sanitize filename
    file_name_clean <- file_name %>%
      str_replace("\\.csv$", "") %>%                  # remove .csv extension if present
      str_replace_all("[^A-Za-z0-9_\\-]", "_")        # replace bad characters
    
    # Drop the source_file column before export
    df <- df %>% select(-source_file)
    
    # Write to file
    write_csv(df, file.path(output_dir, paste0(file_name_clean, ".csv")))
  })


