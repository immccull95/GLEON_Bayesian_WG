# Jake Zwart; 2019-02-07;
# 

library(dplyr) 
library(googledrive)


# load in google drive helper functions that allow downloads directly from our shared google drive folder 
source('RCode/helper_functions/google_drive_functions.R')

# full file path of the google drive file we want to download 
gd_file = 'Sunapee_buoydata/tempstring_data/2007-2017_fulltemprecord_L1-JB.csv' 

# If file doesn't already exist on local comp, retrieve google drive file, write to local comp, and read in file 
sunapee_buoy = google_drive_get(file = gd_file) %>%
  read.csv(.) %>% as_tibble() %>% 
  mutate(datetime = as.POSIXct(as.character(datetime), tz = 'GMT'))

bth_file = 'Sunapee_buoydata/tempstring_data/Sunapee.bth'  # bathymetry data 
sunapee_bth = google_drive_get(file = bth_file) %>%
  read.table(., header = T, sep = ',') %>% as_tibble() %>% 
  rename(depth_m = Bathymetry.Depths, 
         area_m2 = Bathymetry.Areas) 


# writing out altered data locally and posting to google drive. Posting to google drive takes a few minutes depending on file size 
write.csv(sunapee_buoy, file = gd_file, row.names = F)
post = FALSE # post updated files or not to gd 
if(post){
  google_drive_put(file = gd_file) 
}



