
# first, I'm calculating the time offset for each day,
# so that I can more closely match events. even if its not perfect

# the two datasets: there are photos with timestamps from the camera
# and events with timestamps for the emlid

# emlid is considered to be "true" time, whereas the camera time is the one to be adjusted


# ----- 
# thinking of some milisecond time stuff. lets see how this goes. 

# 

library(dplyr)
library(exifr)
library(lubridate)

configure_exiftool(command = "C:\\Users\\X\\Documents\\exiftool-13.24_64\\exiftool_files\\exiftool.pl",
                   perl_path = "C:\\Users\\X\\Documents\\exiftool-13.24_64\\exiftool_files\\perl.exe")

# --- Set paths ---
photo_dir <- "F:\\Bohn2025_1\\EaglePod\\20250609\\SonyILXLR1\\100MSDCF"  # Change to your folder
events_file <- "F:\\Bohn2025_1\\EaglePod\\20250609\\events_pos_CACS\\events_pos\\ReconMini_raw_20250609172043_events.txt"  # Change to your .txt file

outdir <- 'F:\\Bohn2025_1\\EaglePod\\20250609\\Geotagged' # location for calculate offset 

# --- Read photo timestamps ---
photos <- list.files(photo_dir, pattern = "\\.JPG$", full.names = TRUE)

read_exif(photos[1]) %>% colnames()

# lets use just the first 100 photos for calculating the offset. 
photo_data <- read_exif(photos[1:5], tags = c("DateTimeOriginal", "SubSecDateTimeOriginal"))

photo_data

photo_data$SubSecDateTimeOriginal %>% head()

# ---------------

events_data <- read.table(events_file, 
           header = TRUE,
           sep = "", 
           stringsAsFactors = FALSE) %>% 
  as_tibble()

events_data %>% head()

events_data$GPST %>% head()

events_data <- events_data %>%
  mutate(
    # Combine date and time strings
    gpst_string = paste(X., GPST),
    
    # Parse as datetime object in UTC
    #datetime_utc = mdy_hms(gpst_string, tz = "UTC")
  )

events_data$gpst_string %>% head()

# ------------------------- 

# trying to compare: 
library(lubridate)
library(dplyr)

op <- options(digits.secs=6)

# 1. Parse GPST strings (no timezone info, so assume UTC)
events_data <- events_data %>%
  mutate(datetime_utc = ymd_hms(gpst_string, tz = "UTC", truncated = 3))
events_data$GPST %>% head()
events_data$datetime_utc %>% head()


# 2. Parse photo timestamps (these already include timezone info like -05:00)
photo_data <- photo_data %>%
  mutate(datetime_utc = ymd_hms(SubSecDateTimeOriginal, truncated = 3))

photo_data$datetime_utc %>% head()

events_data %>% nrow()
photo_data %>% nrow()

# 4. Calculate time offset (in seconds or milliseconds)
# Assuming one-to-one match row-wise
time_offsets <- photo_data$datetime_utc - events_data[1:nrow(photo_data),]$datetime_utc
time_offsets_sec <- as.numeric(time_offsets, units = "secs")
time_offsets_sec


cbind(photo_data[,-(1:2)], events_data[1:nrow(photo_data),]$datetime_utc)

delta_avg <- time_offsets_sec %>% mean()
delta_avg

# now I think I want to like, export this somewhere? 

# --- Create a summary table with time offset info ---
summary_table <- tibble(
  photo_directory = photo_dir,
  events_file = events_file,
  time_offset_seconds = delta_avg
)

# --- Export to CSV ---
summary_file <- file.path(outdir, "time_offset_summary.csv")
write.csv(summary_table, summary_file, row.names = FALSE)


