# Jake Zwart; 2019-02-07;
# 

library(dplyr) 
library(googledrive)
library(LakeMetabolizer)


# load in google drive helper functions that allow downloads directly from our shared google drive folder 
source('RCode/helper_functions/google_drive_functions.R')

# full file path of the google drive file we want to download 
gd_file = 'Sunapee_buoydata/tempstring_data/2007-2017_fulltemprecord_L1-JB.csv' 

# If file doesn't already exist on local comp, retrieve google drive file, write to local comp, and read in file 
sunapee_buoy = google_drive_get(file = gd_file) %>%
  read.csv(., stringsAsFactors = F) %>% as_tibble() %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'GMT'))

# writing out altered data locally and posting to google drive. Posting to google drive takes a few minutes depending on file size
write = FALSE
if(write){
  write.csv(sunapee_buoy, file = gd_file, row.names = F)
}
post = FALSE # post updated files or not to gd
if(post){
  google_drive_put(file = gd_file)
}

bth_file = 'Sunapee_buoydata/tempstring_data/Sunapee.bth'  # bathymetry data 

sunapee_bth = google_drive_get(file = bth_file) %>%
  read.table(., header = T, sep = ',') %>% as_tibble() %>% 
  rename(depths = Bathymetry.Depths, 
         areas = Bathymetry.Areas) 

gloeo_sites_temp = read.csv('Datasets/Sunapee/SummarizedData/Onset_wtrtemp_60min_2006-2016_Allsites.csv', stringsAsFactors = F) %>% 
  as_tibble() %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'GMT')) 

# load wind data for lake number calc 
wnd_file <- 'Sunapee_buoydata/met_data/2007-2017_wind_L1.csv' 

sunapee_wnd <- google_drive_get(file = wnd_file) %>%
  read.csv(., stringsAsFactors = F) %>% as_tibble() %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'GMT')) %>% 
  rename(wnd = WindSp_ms)

# loading gloeo data to see min and max days of sampling to help guide filtering of DOY for stability metrics 
gloeo_obs = read.csv('Datasets/Sunapee/SummarizedData/All_Sites_Gloeo.csv', stringsAsFactors = F) %>% 
  as_tibble() %>% mutate(datetime = as.POSIXct(date, format = '%Y-%m-%dT%H:%M:%SZ')) 
min(gloeo_obs$dayofyr)
max(gloeo_obs$dayofyr)

## estimating stability metrics for Sunapee based on buoy data 
colnames(sunapee_buoy) <- gsub('TempC', 'wtr', colnames(sunapee_buoy))
colnames(sunapee_buoy) <- gsub('p5m', '.5', colnames(sunapee_buoy))
colnames(sunapee_buoy) <- gsub('1m', '1', colnames(sunapee_buoy))
colnames(sunapee_buoy) <- gsub('2m', '2', colnames(sunapee_buoy))
colnames(sunapee_buoy) <- gsub('3m', '3', colnames(sunapee_buoy))


stability_data <- sunapee_buoy %>% 
  mutate(doy = lubridate::yday(datetime)) %>% 
  dplyr::filter(location == 'loon') %>% 
  select(datetime, starts_with('wtr'))

wnd_data <- left_join(stability_data, dplyr::filter(dplyr::select(sunapee_wnd, datetime, wnd, location), location == 'loon'), by = 'datetime') %>% 
  select(datetime, wnd)
wnd.height = 1.7

sunapee_schmidt <- ts.schmidt.stability(wtr = stability_data, bathy = sunapee_bth, na.rm = T) 
sunapee_ln_na_incl <- ts.lake.number(wtr = stability_data, wnd = wnd_data, 
                             wnd.height = wnd.height, bathy = sunapee_bth)

depths = get.offsets(stability_data)
all.data = merge(stability_data, wnd_data, by = "datetime")
cols = ncol(all.data)
wtr = all.data[, -cols]
wnd = all.data[, c(1, cols)]
n = nrow(stability_data)
ln = rep(NA, n)
wtr.mat = as.matrix(wtr[, -1])
dimnames(wtr.mat) <- NULL
seasonal = TRUE
bathy = sunapee_bth

for (i in 1:n) {
  temps = wtr.mat[i, ]
  
  if (all(is.na(temps)) || is.na(wnd[i, 2])) {
    next
  }
  
  notNA = !is.na(temps)
  
  m.d = meta.depths(wtr.mat[i, notNA], depths[notNA], seasonal = seasonal)
  if (any(is.na(m.d)) || any(is.infinite(m.d))) {
    next
  }

  epi.dens = layer.density(0, m.d[1], wtr.mat[i, notNA], depths[notNA], 
                           bathy$areas, bathy$depths)
  hypo.dens = layer.density(m.d[2], max(depths[notNA]), wtr.mat[i,notNA], depths[notNA], bathy$areas, bathy$depths)
  uS = uStar(wnd[i, 2], wnd.height, epi.dens)
  St = schmidt.stability(wtr.mat[i, notNA], depths[notNA], bathy$areas, 
                         bathy$depths)
  ln[i] = lake.number(bathy$areas, bathy$depths, uS, 
                       St, m.d[1], m.d[2], hypo.dens)
}

sunapee_ln_no_na = data.frame(datetime = rLakeAnalyzer:::get.datetime(wtr), lake.number = ln)


saveRDS(sunapee_ln_no_na, 'Datasets/Sunapee/Stability_metrics/sunapee_lake_number.rds')
saveRDS(sunapee_schmidt, 'Datasets/Sunapee/Stability_metrics/sunapee_schmidt_stability.rds')


