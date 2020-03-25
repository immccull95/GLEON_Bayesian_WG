# Jake Zwart; 2019-10-07;
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
  mutate(datetime = as.POSIXct(datetime, tz = 'GMT'),
         wnd = case_when(!is.na(WindSp_ms) ~ WindSp_ms,
                         is.na(WindSp_ms) & !is.na(AveWindSp_ms) ~ AveWindSp_ms,
                         TRUE ~ WindSp_ms)) %>%
  rename(wnd_dir = AveWindDir_deg)

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


wtr_data <- sunapee_buoy %>% 
  mutate(doy = lubridate::yday(datetime)) %>% 
  dplyr::filter(location == 'loon') %>% 
  select(datetime, starts_with('wtr'))

wnd_data <- left_join(wtr_data, dplyr::filter(dplyr::select(sunapee_wnd, datetime, wnd, wnd_dir, location), location == 'loon'), by = 'datetime') %>% 
  select(datetime, wnd)
wnd.height = 1.7

# U* is the water friction velocity (m s^-1) due to wind stress 
# uStar = rLakeAnalyzer::ts.uStar(wtr = wtr_data, wnd = wnd_data, wnd.height = wnd.height, bathy = sunapee_bth, seasonal = F)
# contains a lot of NA's cuz wtr data frame has a lot of NA's; modifying code slightly below 


na.rm = T
seasonal = F
bathy = sunapee_bth
depths = get.offsets(wtr_data)
all.data = merge(wtr_data, wnd_data, by = "datetime")
cols = ncol(all.data)
wtr = all.data[, -cols]
wnd = all.data[, c(1, cols)]
n = nrow(wtr)
uStar = rep(NA, n)
wtr.mat = as.matrix(wtr[, -1])
dimnames(wtr.mat) <- NULL
for (i in 1:n) {
  if(is.na(wnd[i,2])){
    next
  }
  if (na.rm) {
    temps = wtr.mat[i, ]
    if (all(is.na(temps))) {
      next
    }
    notNA = !is.na(temps)
    
    m.d = meta.depths(temps[notNA], depths[notNA], seasonal = seasonal)
    if (any(is.na(m.d))) {
      if(diff(range(temps[notNA])) < 2){
        m.d[1] = max(depths[notNA])
      }else{
        next
      }
    }
    
    epi.dens = layer.density(0, m.d[1], temps[notNA], depths[notNA], 
                             bathy$areas, bathy$depths)
    
    uStar[i] = uStar(wnd[i, 2], wnd.height, epi.dens)
  }
}
output = data.frame(datetime = rLakeAnalyzer:::get.datetime(wtr), uStar = uStar)

windows()
plot(output$uStar~output$datetime, type = 'l')

saveRDS(output, 'Datasets/Sunapee/wind_energy/sunapee_uStar_instantaneous.rds')


daily = output %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date) %>% 
  summarise(ave_uStar = mean(uStar, na.rm = T)) %>% 
  ungroup()

plot(daily$ave_uStar ~ daily$date, type = 'l')

library(zoo) # library for moving averages, sums, etc... 
moveave = zoo::rollmean(x = daily$ave_uStar, k = 5, align = 'left')
plot(moveave, type ='l')

movesum = zoo::rollsum(x = daily$ave_uStar, k = 5, align = 'left')
plot(movesum, type = 'l')


