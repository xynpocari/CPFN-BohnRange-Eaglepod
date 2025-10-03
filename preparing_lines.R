# lets clean up and combine the layer for the data collection lines

# so these are the lines that were used by the pilot to actually 
# do the data collection. 

# the cleaned version will serve many purposes. 

# ------------------------------------- #

library(sf)
library(dplyr)
library(tibble)

# lets read in the separate layers

EW <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\by_orientation\\BohnLines_EW.shp")
NW <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\by_orientation\\BohnLines_NS.shp")
frontslash <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\by_orientation\\BohnLines_frontslash.shp")
backslash <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\by_orientation\\BohnLines_backslash.shp")

names(EW)
names(NW)
names(frontslash)
names(backslash)

# Add orientation column
EW <- EW %>% mutate(orientation = "EW")
NW <- NW %>% mutate(orientation = "NW")
frontslash <- frontslash %>% mutate(orientation = "frontslash")
backslash <- backslash %>% mutate(orientation = "backslash")

# frontslash is different from the rest. let me change the colnames
frontslash <- frontslash %>%
  rename(dr_ctgr = dir_catego, 
         flght_w = flight_win)

# okay, so I only want to keep a subset of the cols per layer
keep_cols <- c('ID', 'dr_ctgr', 'flght_w', 'length', 'status', 'orientation')

# Subset columns for each layer
EW <- EW %>% select(any_of(keep_cols))
NW <- NW %>% select(any_of(keep_cols))
frontslash <- frontslash %>% select(any_of(keep_cols))
backslash <- backslash %>% select(any_of(keep_cols))

names(EW)
names(NW)
names(frontslash)
names(backslash)

# okay, lets bind 

all_lines <- bind_rows(EW, NW, frontslash, backslash)

all_lines

# and lets reproject
all_lines <- st_transform(all_lines, crs = 2956)
all_lines

# lets export 

st_write(all_lines, 
         'F:\\Bohn2025_1\\DataCollection_Lines\\Bohn_DataCollectionLines.gpkg')

# -------------------------- #

# and lets also export the short lines here 

short <- st_read("F:\\Bohn2025_1\\DataCollection_Lines\\by_orientation\\BohnLines_short.shp")

short <- short %>%
  mutate(ID = fid)

short <- short %>% select(-fid)

short <- st_transform(short, crs = 2956)
short

st_write(short, 
         'F:\\Bohn2025_1\\DataCollection_Lines\\Bohn_DataCollectionLines_Short.gpkg')



