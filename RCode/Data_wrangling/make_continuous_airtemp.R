#Title: Adding continuous Newport air temp data to Gloeo data
#Author: Mary Lofton
#Date: 28APR19

#load packages and source function files
library(tidyverse)
library(lubridate)
source('RCode/NEFI/get_data.R')
source('RCode/helper_functions/plug_n_play_functions.R')

#data options
start_date = '2007-01-01' # in YYYY-MM-DD format; 1st 
end_date = '2016-12-31' 
site = c('midge','coffin','newbury','fichter') # options are midge, coffin, newbury, or fichter 
model_timestep = 7 # model timestep in days if filling in dates
fill_dates = TRUE  # T/F for filling in dates w/o observations with NA's 

#load in Gloeo dataset
dat = plug_n_play_data(start_date = start_date,
                       end_date = end_date,
                       sites = site,
                       model_timestep = model_timestep,
                       fill_dates = fill_dates)  

#get date in good shape for merging
dat1 <- dat %>% 
  mutate(DATE = as.POSIXct(date, format = "%Y-%m-%d"))

dat2 <- dat1 %>%
  mutate(Date = date(DATE)+1)


##reading in weather data
met <- read_csv("./Datasets/Sunapee/RawData/weather/Newport2005-2016_raw.csv") %>% 
  mutate(date = as.POSIXct(DATE, format = "%m/%d/%Y")) %>%
  mutate(Date = date(date)) %>%
  select(Date, TMAX, TMIN, TOBS)
           
#joining datasets
final <- left_join(dat2, met, by = "Date") %>%
  select(-X, -X.1, -DATE, -Date) %>% #get rid of unneccessary columns
  mutate(TMIN = ((TMIN-32)*(5/9)),
         TMAX = ((TMAX-32)*(5/9)),
         TOBS = ((TOBS-32)*(5/9))) #converting to celsius

#checking for temp NAs (hooray!! there aren't any!!)
check <- final %>% filter(is.na(TOBS))

#checking to see if temp looks more or less right
check1 <- subset(final, site == "midge")

tempplot <- ggplot(data = check1, aes(x = date, y = TOBS))+
  geom_line(size = 1)+
  theme_bw()
tempplot

write.csv(final, "./Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp_airtemp.csv", row.names = FALSE)
