library(sf)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)
library(tmap)

op <- options(digits.secs=6)


# read in where we are storing geotagging info
geotag_dir <- 'F:\\Bohn2025_1\\EaglePod\\20250609\\Geotagged'

# Read in the lines
lines <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\Bohn_DataCollectionLines.gpkg")
lines
# right now, lets just try to solve this for day 1 data. 

lines_today <- lines %>% filter(collect_20250609 == 'yes')

lines_today
lines_today %>% nrow()

lines_today <- st_transform(lines_today, crs = 4326)


# now lets read in the photos_pos matches
events <- read.csv(file.path(geotag_dir, 'photos_pos_matches.csv')) %>%
  as_tibble()

# turn this info sf object 
events <- st_as_sf(events, coords = c('longitude.deg.','latitude.deg.'), crs = 4326, remove = FALSE)

tm_shape(events) + tm_dots()

names(events)

events$datetime_utc_photo_offset %>% head()

# and parse the datetime
events <- events %>%
  mutate(
    # Parse as datetime object in UTC
    datetime_utc_photo_offset = ymd_hms(datetime_utc_photo_offset, tz = "UTC", truncated = 3)
  )

events$datetime_utc_photo_offset %>% head()

# okay... now, lets group 
events_group <- events %>%
  arrange(datetime_utc_photo_offset) %>%
  mutate(
    time_diff = as.numeric(difftime(datetime_utc_photo_offset, lag(datetime_utc_photo_offset, default = first(datetime_utc_photo_offset)), units = "secs")),
    time_diff = replace_na(time_diff, 0),
    group = cumsum(time_diff > 10) + 1
  )


events_group

tmap_options(max.categories = 100)

tm_shape(events_group) +
  tm_dots(col = 'group', style = "cat", palette = "Set3", size = 0.1) +
  tm_layout(legend.outside = TRUE)

# hmm. let me try to export this to see it. 

st_write(events_group, 
         file.path(geotag_dir, 'photos_group.gpkg'))

# if there is less than 10 items in the group, remove that group. 
events_group <- events_group %>%
  group_by(group) %>%
  filter(n() >= 10) %>%
  ungroup()

tm_shape(events_group) +
  tm_dots(col = 'group', style = "cat", palette = "Set3", size = 0.1) +
  tm_shape(lines_today) + tm_lines() + 
  tm_layout(legend.outside = TRUE)

st_write(events_group, 
         file.path(geotag_dir, 'photos_group_filter.gpkg'))

# -------------------------------

# now, assinging a line to the photos 
# add a column to photos called "line_ID" 
# the photos are points. the lines are lines. the line_ID should be the "ID" 
# of the nearest line. however, some points are close to multiple lines. 
# so I want to evaluate the closest line for all points within the same group. 
# and then the majority closest line should be assigned to the line_ID of 
# all photos that belong to that group. 
# that is, all photos inside the same group will have the same line_ID. 

# For each event point, find the nearest line
nearest_lines <- st_nearest_feature(events_group, lines_today)

# Add the nearest line ID as a temporary column
events_group$nearest_line_id <- lines_today$ID[nearest_lines]

# Now for each group, find the most common line ID and assign it
events_group <- events_group %>%
  group_by(group) %>%
  mutate(
    line_ID = names(sort(table(nearest_line_id), decreasing = TRUE))[1]
  ) %>%
  ungroup()

events_group

# lets export to check this. 
st_write(events_group, 
         file.path(geotag_dir, 'photos_group_filter_lineID.gpkg'))

# and lets also export that as csv
events_group_tib <- st_drop_geometry(events_group)
write.csv(events_group_tib, 
          file.path(geotag_dir, 'photos_group_filter_lineID.csv'), 
          row.names = F)

# ----------------------------------------------

# okay, now next I want to copy photos into their respective folders. 
# so in the events_group_tib, there is a col called line_ID. 
# I want to copy each photo (location is 'path') into the subfolder which 
# is in the output folder, and the subfolder will be named the line_ID 

events_group_tib$line_ID %>% head()

output_folder <- 'F:\\Bohn2025_1\\Geotagging\\Lines_by_ID'


# Unique line_IDs in the tibble
line_ids <- unique(events_group_tib$line_ID)

# Get list of existing folders in output path
existing_folders <- list.dirs("F:/Bohn2025_1/Geotagging/Lines_by_ID", full.names = FALSE, recursive = FALSE)

# Find line_IDs with no matching folder
missing_folders <- setdiff(line_ids, existing_folders)

# Show result
if (length(missing_folders) == 0) {
  message("✅ All line_IDs have matching folders.")
} else {
  warning("⚠️ The following line_IDs do NOT have matching folders:\n",
          paste(missing_folders, collapse = ", "))
}

# Loop over each line_ID
for (line in unique(events_group_tib$line_ID)) {
  # Filter for this line_ID
  line_photos <- events_group_tib %>% filter(line_ID == line)
  
  # Copy photos
  dest_folder <- file.path(output_folder, line, "photos")
  
  moved <- file.rename(from = line_photos$path,
                       to = file.path(dest_folder, basename(line_photos$path)))
  
  # Print message if any were copied
  if (any(moved)) {
    message("📸 moved ", sum(moved), " photo(s) to: ", dest_folder)
  }
  
}








