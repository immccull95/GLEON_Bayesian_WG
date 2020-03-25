############################ Lake Sunapee IN situ data exploration #############################
# Date: 1-1-18
# updated:10-5-18 by IMM
# Authors: JAB, MEL
################################################################################################
### Gloeo exploratory analysis
#Created 1 January 2018 - JAB

# Install R Packages ####
library(plyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(doBy) #included by LSB to aggregate water temp data following Bethel Steele code.

# Wind direction packages
install.packages("openair")
library(openair)

# Not currently using these wind packages
install.packages("rWind")
devtools::install_github("jabiologo/rWind")  
library(rWind)

install.packages('googledrive')
library(googledrive)

#### Read in data for all sites from Shannon weekly summary ####
#JAB updated Shannon weekly summary to include only 1 observation per week
#Sheet tells R what to pull from on the excel document which is handy insted of
#loading multiple csv's
coffin_gloeo = read_excel("Datasets/Sunapee/Level1/Sunapee_weeklysummary_JBedits.xlsx", sheet='coffin_weeklygloeo')
fichter_gloeo = read_excel("Datasets/Sunapee/Level1/Sunapee_weeklysummary_JBedits.xlsx", sheet='fichter_weeklygloeo')
midge_gloeo = read_excel("Datasets/Sunapee/Level1/Sunapee_weeklysummary_JBedits.xlsx", sheet='midge_weeklygloeo')
newbury_gloeo = read_excel("Datasets/Sunapee/Level1/Sunapee_weeklysummary_JBedits.xlsx", sheet='newbury_weeklygloeo')

#check the data structure
str(midge_gloeo)

#Merge datasets for all sites into one file
all_sites_gloeo = bind_rows(coffin_gloeo,fichter_gloeo,midge_gloeo,newbury_gloeo)
write_csv(all_sites_gloeo, "All_Sites_Gloeo.csv")
summary(all_sites_gloeo)

#Merge in-situ data from newbury and midge with gloeo data
midge_insitu = read_excel("Datasets/Sunapee/Level1/Sunapee_weeklysummary_JBedits.xlsx", sheet='midge_insitu_data')
newbury_insitu = read_excel("Datasets/Sunapee/Level1/Sunapee_weeklysummary_JBedits.xlsx", sheet='newbury_insitu_data')

#Add in week 
midge_insitu_week <- midge_insitu %>%
  mutate(year = year(date)) %>%
  mutate(week = week(date))

newbury_insitu_week <- newbury_insitu %>%
  mutate(year = year(date)) %>%
  mutate(week = week(date))

midge_all = full_join(midge_gloeo,midge_insitu_week,by = c("year", "week"))
newbury_all = full_join(newbury_gloeo,newbury_insitu_week,by = c("year", "week"))

write_csv(midge_all,"midge_all.csv")
write_csv(newbury_all,"newbury_all.csv")

# Read in final gloeo data ####
gloeo = read_csv("Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp-interp.csv")

#Convert sites to factor
sites <- c("Coffin", "Midge", "Fichter", "Newbury")
gloeo$site <- parse_factor(gloeo$site, levels = sites)

str(gloeo)

############################# WATER TEMP AGGREGATION #################################
#Updated by LSB 26-June-2018

#### Read in water temp data ####
watertemp_hourly = read_csv("Datasets/Sunapee/Level1/temp_2006-2016_L1_20Oct2017.csv", col_types = cols(
  coffin = col_double(),
  fichter = col_double(),
  newbury = col_double()))
str(watertemp_hourly)

onset_water_temp = read_csv("Datasets/Sunapee/SummarizedData/Onset_wtrtemp_60min_2006-2016_Allsites.csv", col_types = cols(
  coffin = col_double(),
  fichter = col_double(),
  newbury = col_double(),
  midge = col_double()))
str(watertemp_hourly)

watertemp_HOBO <- read_csv("Datasets/Sunapee/SummarizedData/HOBO_light_wtrtemp_10min_2009-2016_Allsites.csv", col_types = cols(
  datetime = col_datetime(format = ""),
  date = col_date(format = ""),
  time = col_time(format = ""),
  dayofyr = col_double(),
  temp_Coffin = col_double(),
  temp_Fichter = col_double(),
  temp_Midge = col_double(),
  temp_OldNewbury = col_double(),
  light_Coffin = col_double(),
  light_Fichter = col_double(),
  light_Midge = col_double(),
  light_OldNewbury = col_double()))

str(watertemp_HOBO)

watertemp_HOBO_day <- watertemp_HOBO %>% 
  mutate(day = day(date))

# 2009 data - readings every 30 min so filtered out to only include hourly readings
onset_watertemp_hourly <- onset_water_temp %>% 
  mutate(minute = minute(datetime)) %>% 
  filter(minute == 0) %>% 
  select(-minute)

#### calculate weekly, monthly and  annual summaries ####
#Add in week, month and year
temp_L1 <-onset_watertemp_hourly %>%
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date)) %>%  #this way we are considering week numbers as the the number of complete seven day periods that have occurred between the date and January 1st, plus one.
  mutate(day = day(date))

#Aggregate by week number, month, year and sampling period
sumfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), min=min(x, na.rm=TRUE, ...), max=max(x, na.rm=TRUE, ...), 
    median=median(x, na.rm=TRUE, ...),sd = sd(x, na.rm=TRUE, ...), obs=sum(!is.na(x)))}
temp_L1 <- as.data.frame(temp_L1)
str(temp_L1)

watertemp_HOBO_day <- as.data.frame(watertemp_HOBO_day)
str(watertemp_HOBO_day)

onset_watertemp_day <- summaryBy(coffin + fichter + newbury + midge ~ date, data=temp_L1, FUN=sumfun)

hobo_watertemp_day <- summaryBy(temp_Coffin + temp_Fichter + temp_OldNewbury + temp_Midge ~ date, data=watertemp_HOBO_day, FUN=sumfun)

write_csv(watertemp_day, "Datasets/Sunapee/SummarizedData/HOBO_wtrtemp_daily_summary_2009-2016_Allsites.csv")

onset_watertemp_week <- summaryBy(coffin + fichter + newbury + midge ~ year + week, data=temp_L1, FUN=sumfun)

watertemp_month <- summaryBy(coffin + fichter + newbury + midge ~ year + month, data=temp_L1, FUN=sumfun)

#Remove Inf and -Inf
onset_watertemp_day <- replace(onset_watertemp_day,onset_watertemp_day == Inf|onset_watertemp_day == -Inf, NA)

onset_watertemp_week <- replace(onset_watertemp_week,onset_watertemp_week == Inf|onset_watertemp_week == -Inf, NA)

#drop days with less than 18 obs (75% of the data) -----
ix=which(watertemp_day$coffin.obs <18)
for (i in c('coffin.min', 'coffin.max', 'coffin.mean', 'coffin.median')) {watertemp_day[ix,i]=NA}

ix=which(watertemp_day$fichter.obs <18)
for (i in c('fichter.min', 'fichter.max', 'fichter.mean', 'fichter.median')) {watertemp_day[ix,i]=NA}

ix=which(watertemp_day$newbury.obs <18)
for (i in c('newbury.min', 'newbury.max', 'newbury.mean', 'newbury.median')) {watertemp_day[ix,i]=NA}

ix=which(watertemp_day$midge.obs <18)
for (i in c('midge.min', 'midge.max', 'midge.mean', 'midge.median')) {watertemp_day[ix,i]=NA}


#drop weeks with less than 126 obs (75% of the data) -----
ix=which(watertemp_week$coffin.obs <126)
for (i in c('coffin.min', 'coffin.max', 'coffin.mean', 'coffin.median')) {watertemp_week[ix,i]=NA}

ix=which(watertemp_week$fichter.obs <126)
for (i in c('fichter.min', 'fichter.max', 'fichter.mean', 'fichter.median')) {watertemp_week[ix,i]=NA}

ix=which(watertemp_week$newbury.obs <126)
for (i in c('newbury.min', 'newbury.max', 'newbury.mean', 'newbury.median')) {watertemp_week[ix,i]=NA}

ix=which(watertemp_week$midge.obs <126)
for (i in c('midge.min', 'midge.max', 'midge.mean', 'midge.median')) {watertemp_week[ix,i]=NA}

#drop months with less than 75% of the observations -----
dats<-as.Date(paste0(watertemp_month$year,'-',watertemp_month$month,'-10'),'%Y-%m-%d')
watertemp_month$totalobs<-days_in_month(dats)*24

ix=which(watertemp_month$coffin.obs < (0.75*watertemp_month$totalobs))
for (i in c('coffin.min', 'coffin.max', 'coffin.mean', 'coffin.median')) {watertemp_month[ix,i]=NA}

ix=which(watertemp_month$fichter.obs < (0.75*watertemp_month$totalobs))
for (i in c('fichter.min', 'fichter.max', 'fichter.mean', 'fichter.median')) {watertemp_month[ix,i]=NA}

ix=which(watertemp_month$newbury.obs < (0.75*watertemp_month$totalobs))
for (i in c('newbury.min', 'newbury.max', 'newbury.mean', 'newbury.median')) {watertemp_month[ix,i]=NA}

ix=which(watertemp_month$midge.obs < (0.75*watertemp_month$totalobs))
for (i in c('midge.min', 'midge.max', 'midge.mean', 'midge.median')) {watertemp_month[ix,i]=NA}

##aggregate years using only months from June to August -----
filtered<-subset(temp_L1,temp_L1$month>5 & temp_L1$month<9)
summary(filtered)
watertemp_year <- summaryBy(coffin + fichter + newbury + midge ~ year, data=filtered, FUN=sumfun)

#drop years with less than 75% of observations
ix=which(watertemp_year$coffin.obs < (0.75*92*24)) #92 number of days from May to September
for (i in c('coffin.min', 'coffin.max', 'coffin.mean', 'coffin.median')) {watertemp_year[ix,i]=NA}

ix=which(watertemp_year$fichter.obs < (0.75*92*24))
for (i in c('fichter.min', 'fichter.max', 'fichter.mean', 'fichter.median')) {watertemp_year[ix,i]=NA}

ix=which(watertemp_year$newbury.obs < (0.75*92*24))
for (i in c('newbury.min', 'newbury.max', 'newbury.mean', 'newbury.median')) {watertemp_year[ix,i]=NA}

ix=which(watertemp_year$midge.obs < (0.75*92*24))
for (i in c('midge.min', 'midge.max', 'midge.mean', 'midge.median')) {watertemp_year[ix,i]=NA}

##aggregate by sampling period ----
  #calculate average values for the period that we have Gloeo data
#stipulating the begining and the end of sampling seasons for each site in each year
samplingfun <- function(x, ...){
  c(min=min(x, na.rm=TRUE, ...), max=max(x, na.rm=TRUE, ...))}
all_sites_gloeo <- as.data.frame(all_sites_gloeo)
gloeo_samplingperiod <- summaryBy(dayofyr ~ site + year, data=all_sites_gloeo, FUN=samplingfun)

#filtering data
filt_gloeo<-temp_L1
for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Midge' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$midge[ex]=NA
  }

for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Coffin' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$coffin[ex]=NA
}

for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Fichter' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$fichter[ex]=NA
}

for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Newbury' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$newbury[ex]=NA
}

#sumarising
watertemp_sampling <- summaryBy(coffin + fichter + newbury + midge ~ year, data=filt_gloeo, FUN=sumfun)


##Save data into .csv files ----
write.csv(watertemp_day,"Datasets/Sunapee/SummarizedData/watertemp_day.csv", row.names = F)
write_csv(watertemp_week,"Datasets/Sunapee/Level1/watertemp_week.csv")
write_csv(watertemp_month,"Datasets/Sunapee/Level1/watertemp_month.csv")
write_csv(watertemp_year,"Datasets/Sunapee/Level1/watertemp_year.csv")
write_csv(watertemp_sampling,"Datasets/Sunapee/Level1/watertemp_sampling.csv")

#Comented by LSB 14-May-2015
# # this step was for merging the sites together
# #Convert water temp to long data
# watertemp_long <- watertemp %>%
#   select(week:newbury.median) %>%
#   filter(!is.na(coffin.mean)) %>%
#   gather(key=site, value = watertemp_c, coffin.mean:newbury.median) %>%
#   separate(col=site,into = c("site","method")) %>%
#   spread(key=method,value=watertemp_c) %>%
#   arrange(year,site)
# 
# watertemp_all_long <- bind_rows(watertemp_long,watertemp_midge) %>%
#   arrange(year,site)
# 
# write_csv(watertemp_all_long,"watertemp_all_long.csv")  




#### Read in light dataset ####

light_allsites <- read_excel("Datasets/Sunapee/Level1/templight_0916_L1_4sites_30Oct2017-JBedits.xlsx", sheet = "templight_0916_L1_4sites_30Oct2")#(
  #temp_Coffin = col_double(),
  #temp_Fichter = col_double(),
  #temp_OldNewbury = col_double(),
  #light_Coffin = col_double(),
  #light_Fichter = col_double(),
  #light_OldNewbury = col_double()))

str(light_allsites)

light_allsites <- light_allsites %>% 
  mutate(week = week(datetime)) %>%
  mutate(week_day = wday(datetime)) %>% 
  mutate(day = day(datetime))

# Code to consolidate 10 min light and temp data to same 10 min reading

# try to remove NA values first

light_allsites_noNA <- light_allsites[!is.na(light_allsites$temp_Coffin),]


#Count number of observations for each column

light_allsites_count <- light_allsites %>% 
  mutate(week = week(datetime)) %>%
  select(year,temp_Coffin:week) %>% 
  group_by(year,week) %>% 
  summarize(count = n())

#Separate out 10 min readings at 0 min vs. 7 min 2 min 9 min 8 1 min 3 4min
light_10min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(0,10,20,30,40,50))

write_csv(light_10min,"Datasets/Sunapee/Level1/light_10min.csv")

light_10min <- read_csv("Datasets/Sunapee/Level1/light_10min.csv", col_types = cols(
  temp_Coffin = col_double(),
  temp_Fichter = col_double(),
  temp_OldNewbury = col_double(),
  light_Coffin = col_double(),
  light_Fichter = col_double(),
  light_OldNewbury = col_double()))

str(light_10min)

light_9min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(9,19,29,39,49,59))

light_8min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(8,18,28,38,48,58))

light_7min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(7,17,27,37,47,57))

light_6min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(6,16,26,36,46,56))

light_5min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(5,15,25,35,45,55))

light_4min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(4,14,24,34,44,54))

light_3min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(3,13,23,33,43,53))

light_2min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(2,12,22,32,42,52))

light_1min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(1,11,21,31,41,51))

write_csv(light_1min,"Datasets/Sunapee/Level1/light_1min.csv")


# Convert time to 10 min data so all collected at the same time interval

newtime <- as.POSIXlt(light_2min$datetime, format = "%Y-%m-%d %H:%M")
newtime <- (light_2min$datetime)
head(newtime)


for (i in seq_along(newtime)) {
  if (newtime[i]$hour < 24) {
    newtime[i]$min =  newtime[i]$min - 2
  }
  
}

light_9min_newtime = cbind(newtime,light_9min)
light_8min_newtime = cbind(newtime,light_8min)
light_7min_newtime = cbind(newtime,light_7min)
light_6min_newtime = cbind(newtime,light_6min)
light_5min_newtime = cbind(newtime,light_5min)
light_4min_newtime = cbind(newtime,light_4min)
light_3min_newtime = cbind(newtime,light_3min)
light_2min_newtime = cbind(newtime,light_2min)

#Join datasets by newtime
light_newtime <- full_join(light_8min_newtime, light_6min_newtime)
light_newtime2 <- full_join(light_5min_newtime, light_4min_newtime)
light_newtime3 <- full_join(light_3min_newtime, light_2min_newtime)

light_newtime4 <- full_join(light_newtime, light_newtime2)
light_newtime5 <- full_join(light_newtime4, light_newtime3)
light_newtime6 <- full_join(light_newtime5, light_9min_newtime)

light_newtime7 <- full_join(light_10min, light_newtime6, by = c("newtime"))
write_csv(light_newtime7,"Datasets/Sunapee/R Work/Level 1/light_newtime_joinall.csv")

ficht <- sum(is.na(light_newtime6$light_OldNewbury))
ficht2 <- sum(!is.na(light_newtime6$light_OldNewbury))

View(ficht)

write_csv(light_newtime5,"Datasets/Sunapee/Level1/light_newtime_joinall.csv")


#Calculate weekly median, mean, max for light & HOBO temp data at all sites

light_summary_10min <- light_10min %>% 
  mutate(month = month(datetime)) %>% 
  mutate(week = week(datetime)) %>% 
  group_by(year,week) %>% 
  summarize_at(vars(temp_Coffin:light_OldNewbury),funs(mean, median, max (.,na.rm=T)))
  
write_csv(light_summary_10min,"Datasets/Sunapee/Level1/light_temp_weekly_summary_10min_take2.csv")


# Read in summarize dataset with -Inf changed to NA

# does not exist:
light_summary <- read_csv("Datasets/Sunapee/Level1/light_temp_weekly_summary.csv")

# correct file?
light_summary <- read_csv("Datasets/Sunapee/Level1/light_temp_weekly_summary_10min_take2.csv")

light_temp_long <- light_summary %>%
  gather(key=site, value = light, temp_Coffin_mean:light_OldNewbury_max) %>%
  separate(col=site,into = c("measure","site","method")) %>%
  spread(key=method,value=light) %>%
  arrange(year,site)

write_csv(light_temp_long,"Datasets/Sunapee/Level1/light_temp_weekly_summary-long.csv")

#Filter for just Midge light data for now
midge_light <- light_temp_long %>% 
  filter(site=="Midge",measure=="light")

# Finalize light and temp 10 min data ####

# Read in close to final dataset
light_temp <- read_csv("Datasets/Sunapee/Level1/HOBO_Light_temp_10min_2009-2016_Allsites.csv", col_types = cols(
  temp_Coffin = col_double(),
  temp_Fichter = col_double(),
  temp_OldNewbury = col_double(),
  light_Coffin = col_double(),
  light_Fichter = col_double(),
  light_OldNewbury = col_double()))

write.csv(light_temp, "Datasets/Sunapee/Level1/HOBO_Light_temp_10min_2009-2016_Allsites_newdt.csv", row.names = F)

str(light_temp)

#Read in 1 min readings for 17-25 June
# file does not exist:
light_temp_1min <- read_csv("Datasets/Sunapee/Level1/light_temp_1min_Readings.csv", col_types = cols(
  temp_Coffin = col_double(),
  temp_Fichter = col_double(),
  temp_OldNewbury = col_double(),
  light_Coffin = col_double(),
  light_Fichter = col_double(),
  light_OldNewbury = col_double()))

str(light_temp_1min)

#Filter out midge light & temp already at 10 min

light_temp_1min <- light_temp_1min %>% 
  mutate(minute = minute(datetime)) %>% 
  filter(minute %in% c(0,10,20,30,40,50))

write_csv(light_temp_1min,"light_temp_1min_filtered.csv")


#### calculate weekly, monthly and  annual summaries ####
# read in final HOBO light dataset

temp_light_L1 <- read_csv("Datasets/Sunapee/SummarizedData/HOBO_light_wtrtemp_10min_2009-2016_Allsites.csv", col_types = cols(
  temp_Coffin = col_double(),
  temp_Fichter = col_double(),
  temp_Midge = col_double(),
  temp_OldNewbury = col_double(),
  light_Coffin = col_double(),
  light_Fichter = col_double(),
  light_Midge = col_double(),
  light_OldNewbury = col_double()))


#Select only light data and add in week, month and year
light_L1 <- temp_light_L1 %>%
  select(datetime, date, starts_with("light")) %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date)) #this way we are considering week numbers as the the number of complete seven day periods that have occurred between the date and January 1st, plus one.

#Aggregate by week number, month, year and sampling period - SUM for light data instead of mean and no min
sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...),max=max(x, na.rm=TRUE, ...), 
    median=median(x, na.rm=TRUE, ...), obs=sum(!is.na(x)))}
light_L1 <- as.data.frame(light_L1)
str(light_L1)

light_day <- summaryBy(light_Coffin + light_Fichter + light_OldNewbury + light_Midge ~ date, data=light_L1, FUN=sumfun)

light_week <- summaryBy(light_Coffin + light_Fichter + light_OldNewbury + light_Midge ~ year + week, data=light_L1, FUN=sumfun)

light_month <- summaryBy(light_Coffin + light_Fichter + light_OldNewbury + light_Midge ~ year + month, data=light_L1, FUN=sumfun)

#drop days with less than 75% of the data = 144 obs/day * 0.75 = 108 -----
ix=which(light_day$light_Coffin.obs <108)
for (i in c('light_Coffin.max', 'light_Coffin.sum', 'light_Coffin.median')) {light_day[ix,i]=NA}

ix=which(light_day$light_Fichter.obs <108)
for (i in c('light_Fichter.max', 'light_Fichter.sum', 'light_Fichter.median')) {light_day[ix,i]=NA}

ix=which(light_day$light_OldNewbury.obs <108)
for (i in c('light_OldNewbury.max', 'light_OldNewbury.sum', 'light_OldNewbury.median')) {light_day[ix,i]=NA}

ix=which(light_day$light_Midge.obs <108)
for (i in c('light_Midge.max', 'light_Midge.sum', 'light_Midge.median')) {light_day[ix,i]=NA}

####

#drop weeks with less than 75% of the data = 1008 obs/week * 0.75 = 756 -----
ix=which(light_week$light_Coffin.obs <756)
for (i in c('light_Coffin.max', 'light_Coffin.sum', 'light_Coffin.median')) {light_week[ix,i]=NA}

ix=which(light_week$light_Fichter.obs <756)
for (i in c('light_Fichter.max', 'light_Fichter.sum', 'light_Fichter.median')) {light_week[ix,i]=NA}

ix=which(light_week$light_OldNewbury.obs <756)
for (i in c('light_OldNewbury.max', 'light_OldNewbury.sum', 'light_OldNewbury.median')) {light_week[ix,i]=NA}

ix=which(light_week$light_Midge.obs <756)
for (i in c('light_Midge.max', 'light_Midge.sum', 'light_Midge.median')) {light_week[ix,i]=NA}


#drop months with less than 75% of the observations -----
dats<-as.Date(paste0(light_month$year,'-',light_month$month,'-10'),'%Y-%m-%d')
light_month$totalobs<-days_in_month(dats)*144 *0.75

ix=which(light_month$light_Coffin.obs  < light_month$totalobs)
for (i in c('light_Coffin.max', 'light_Coffin.sum', 'light_Coffin.median')) {light_month[ix,i]=NA}

ix=which(light_month$light_Fichter.obs < light_month$totalobs)
for (i in c('light_Fichter.max', 'light_Fichter.sum', 'light_Fichter.median')) {light_month[ix,i]=NA}

ix=which(light_month$light_OldNewbury.obs  < light_month$totalobs)
for (i in c('light_OldNewbury.max', 'light_OldNewbury.sum', 'light_OldNewbury.median')) {light_month[ix,i]=NA}

ix=which(light_month$light_Midge.obs  < light_month$totalobs)
for (i in c('light_Midge.max', 'light_Midge.sum', 'light_Midge.median')) {light_month[ix,i]=NA}

##aggregate years using only months from June to August -----
filtered<-subset(temp_L1,temp_L1$month>5 & temp_L1$month<9)
summary(filtered)
watertemp_year <- summaryBy(coffin + fichter + newbury + midge ~ year, data=filtered, FUN=sumfun)

#drop years with less than 75% of observations
ix=which(watertemp_year$coffin.obs < (0.75*92*24)) #92 number of days from May to September
for (i in c('coffin.min', 'coffin.max', 'coffin.mean', 'coffin.median')) {watertemp_year[ix,i]=NA}

ix=which(watertemp_year$fichter.obs < (0.75*92*24))
for (i in c('fichter.min', 'fichter.max', 'fichter.mean', 'fichter.median')) {watertemp_year[ix,i]=NA}

ix=which(watertemp_year$newbury.obs < (0.75*92*24))
for (i in c('newbury.min', 'newbury.max', 'newbury.mean', 'newbury.median')) {watertemp_year[ix,i]=NA}

ix=which(watertemp_year$midge.obs < (0.75*92*24))
for (i in c('midge.min', 'midge.max', 'midge.mean', 'midge.median')) {watertemp_year[ix,i]=NA}

##aggregate by sampling period ----
#calculate average values for the period that we have Gloeo data
#stipulating the begining and the end of sampling seasons for each site in each year
samplingfun <- function(x, ...){
  c(min=min(x, na.rm=TRUE, ...), max=max(x, na.rm=TRUE, ...))}
all_sites_gloeo <- as.data.frame(all_sites_gloeo)
gloeo_samplingperiod <- summaryBy(dayofyr ~ site + year, data=all_sites_gloeo, FUN=samplingfun)

#filtering data
filt_gloeo<-temp_L1
for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Midge' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$midge[ex]=NA
}

for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Coffin' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$coffin[ex]=NA
}

for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Fichter' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$fichter[ex]=NA
}

for (i in 2005:2016) {
  ix=which(gloeo_samplingperiod$site == 'Newbury' & gloeo_samplingperiod$year == i)
  start<-gloeo_samplingperiod$dayofyr.min[ix]
  end<-gloeo_samplingperiod$dayofyr.max[ix]
  ex=which(filt_gloeo$year == i & filt_gloeo$dayofyr < start & filt_gloeo$dayofyr > end)
  filt_gloeo$newbury[ex]=NA
}

#sumarising
watertemp_sampling <- summaryBy(coffin + fichter + newbury + midge ~ year, data=filt_gloeo, FUN=sumfun)


##Save data into .csv files ----
write.csv(light_day,"Datasets/Sunapee/SummarizedData/light_day_HOBO_aggregation.csv", row.names = F)
write.csv(light_week,"Datasets/Sunapee/SummarizedData/light_week_HOBO_aggregation.csv", row.names = F)
write.csv(light_month,"Datasets/Sunapee/SummarizedData/light_month_HOBO_aggregation.csv", row.names = F)


############### FIGURES ###############

#### Figures for gloeo all sites - facet wrap by year ####
#I plotted day of year on the x axis and total colonies per L on the y
#colors are for the different sites and each panel represents a different year
#the rest of the code below that is mostly formatting for a pretty figure
ggplot(gloeo_Midge, aes(x=week,y=log_gloeo))+
  geom_line(size=1.5,color="green")+
  geom_point()+
  #scale_colour_manual(values=c("black","forestgreen","blue","orange"))+ 
  #scale_x_datetime(breaks = seq(as.POSIXct("2005-08-01"),as.POSIXct("2016-10-01"), by="1 year"),date_labels="%Y")+ #%d-%b-%y
  scale_x_continuous(limits=c(7,42), breaks=seq(10,40, by=5))+ #%d-%b-%y
  labs(y="Log Gloeotrichia Surface Abundance (colonies/L)", x="Week", title = "Midge")+
  facet_wrap(~year)+ #,scales ="free_y")+
  theme_bw(base_family = "Times")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        strip.text = element_text(size=24),
        axis.text.y = element_text(size=18,color="black"),
        axis.text.x = element_text(size=18,color="black"),
        title = element_text(size=20),
        legend.text= element_text(size=18))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="darkslategray2"))

#Saves figure as a pdf with set dimensions in the working directory folder
ggsave("Datasets/Sunapee/Data Visualizations/All_Sites_gloeo-byyear_2005-2016.pdf",width=15, height=8.5)

ggsave("~/Desktop/Midge_log_gloeo_week_2005-2016.pdf",width=11, height=8.5)
ggsave("~/Desktop/Midge_gloeo_day_2005-2016.pdf",width=11, height=8.5)
ggsave("~/Desktop/Midge_gloeo_week_2005-2016.pdf",width=11, height=8.5)

#### All sites - facet wrap by site ####
#This makes each site its own panel and has the different years on the x-axis
ggplot(all_sites_gloeo, aes(x=date,y=totalperL))+
  geom_line(color="green3")+
  scale_x_datetime(breaks = seq(as.POSIXct("2005-08-01"),as.POSIXct("2016-10-01"), by="1 year"),date_labels="%Y")+ #%d-%b-%y
  #scale_y_continuous(limits=c(9.5,20.6), breaks=seq(10,20, by=2))+
  labs(y="Gloeotrichia Surface Abundance (colonies/L)", x=" ")+
  facet_wrap(~site,scales ="free_y")+
  theme_bw(base_family = "Times")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        strip.text = element_text(size=24),
        axis.text.y = element_text(size=18,color="black"),
        axis.text.x = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="darkslategray2"))        

ggsave("Datasets/Sunapee/Data Visualizations/All_Sites_gloeo_2005-2016.pdf",width=15, height=8.5)


#### Individual site and single year figures ####
#Filter for high year = 2013
midge_gloeo_2013 <- midge_gloeo %>%
  filter(year==2013)%>%
  filter(week>19)%>%
  filter(week<39)

ggplot(midge_gloeo_2013, aes(x=date,y=totalperL))+
  geom_line(color="green")+
  #scale_x_datetime(breaks = seq(as.POSIXct("2005-08-01"),as.POSIXct("2016-10-01"), by="1 year"),date_labels="%Y")+ #%d-%b-%y
  #scale_y_continuous(limits=c(9.5,20.6), breaks=seq(10,20, by=2))+
  labs(y="Gloeo Abundance (colonies/L)", x=" ")+
  #annotate("segment",x=as.POSIXct("2008-04-25"),xend=as.POSIXct("2008-04-25"),y=9.5,yend=20,linetype=2,size=1)+
  theme_bw(base_family = "Times")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        axis.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())      


#### Water Temp Figure ####
#Read in data combined and numeric
#THIS PATH/FILE SHOULD BE UPDATED:
watertemp = read.csv("Sunapee_watertemp.csv")
str(watertemp)
summary(watertemp)
watertemp_2007 = filter(watertemp,year>2007)

ggplot(watertemp_2007, aes(x=week,y=coffin.min))+
  geom_line(size=1.5)+
  geom_line(aes(x=week,y=fichter.min),size=1.5,color="forestgreen")+
  geom_line(aes(x=week,y=newbury.min),size=1.5,color="orange")+
  geom_line(aes(x=week,y=midge.min),size=1.5,color="blue")+
  #scale_x_datetime(breaks = seq(as.POSIXct("2005-08-01"),as.POSIXct("2016-10-01"), by="1 year"),date_labels="%Y")+ #%d-%b-%y
  #scale_x_continuous(limits=c(47,300), breaks=seq(50,300, by=50))+ #%d-%b-%y
  #scale_y_continuous(limits=c(9.5,20.6), breaks=seq(10,20, by=2))+
  labs(y="Minimum Water Temperature (°C)", x="Week")+
  facet_wrap(~year)+
  theme_bw(base_family = "Times")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        strip.text = element_text(size=24),
        axis.text.y = element_text(size=18,color="black"),
        axis.text.x = element_text(size=18,color="black"),
        legend.title = element_text(size=20),
        legend.text= element_text(size=18))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="darkslategray2"))        

ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/All_Sites_minwatertemp-byyear_2007-2016.pdf",width=15, height=8.5)


#### Correlation Analysis ####

#cor() pearson used by default but can call spearman or kendall

# Data Exploration for light data ####
#setwd("~/Documents/GLEON_Bayesian_WG/Datasets/Sunapee/SummarizedData")

# Read in gloeo & light data
gloeo <- read_csv("Datasets/Sunapee/SummarizedData/All_Sites_Gloeo.csv")
str(gloeo)

gloeo$date <- as_date(gloeo$date,"%Y-%m-%d")
str(gloeo)

light_day <- read_csv("Datasets/Sunapee/SummarizedData/light_day_HOBO_aggregation.csv", 
                      col_types = cols(date = col_date()))

str(light_day)

#convert light day to long format
light_day_long <- light_day %>%
  gather(key = "metric", value = "light", -date) %>% #gather all variables except datetime
  separate(metric,into = c("value","site"), sep = "_") %>% 
  separate(site,into = c("site","metric")) %>% 
  select(-value) %>% #remove the wtr column
  spread(key = metric, value = light) %>% 
  arrange(site, date) %>% 
  #filter(obs > 108) %>% 
  rename(light_max = max, light_median = median, light_sum = sum) %>% 
  mutate(year = year(date)) 

str(light_day_long)

# Filter by site
light_day_long_site <- light_day_long %>% 
  #filter(site=="Coffin")
  #filter(site=="Fichter")
  #filter(site=="Midge")
  filter(site=="Newbury")

# Figure for light data with site colored and year faceted
ggplot(light_day_long_site, aes(x=date,y=light_sum, color = site))+
  geom_line(size=1.5)+
  facet_wrap(~year,scales ="free_x")+
  labs(title = "Old Newbury")

ggsave("Datasets/Sunapee/Data Visualizations/Light/Coffin-DailyLight_2009-2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Fichter-DailyLight_2009-2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Midge-DailyLight_2009-2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/OldNewbury-DailyLight_2009-2016.pdf",width=15, height=8.5)

#Light data - switch site to Newbury instead of OldNewbury
light_day_long_newb <- light_day_long %>% 
  filter(site =="OldNewbury") %>% 
  mutate(site = "Newbury")
gloeo_light <- left_join(gloeo,light_day_long_newb, by = c("date", "site", "year"))


#join gloeo & light
gloeo_light <- left_join(gloeo,light_day_long, by = c("date", "site", "year"))

gloeo_light_site <- gloeo_light %>% 
  #filter(site=="Coffin")
  #filter(site=="Fichter")
  #filter(site=="Midge")
  filter(site=="Newbury")

# Figure for gloeo vs. date with color gradient for light 
ggplot(gloeo_light_site, aes(x=date,y=totalperL, color = light_sum))+
  geom_point(size=2)+
  facet_wrap(~year,scales ="free")+
  labs(title = "Newbury")

ggsave("Datasets/Sunapee/Data Visualizations/Light/Coffin-Gloeo_Light-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Fichter-Gloeo_Light-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Midge-Gloeo_Light-gradient-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Newbury-Gloeo_Light-gradient-2005_2016.pdf",width=15, height=8.5)

# Figure for gloeo vs. light by each year
gloeo_light_site <- gloeo_light %>% 
  #filter(site=="Coffin")
  #filter(site=="Fichter")
  #filter(site=="Midge")
  filter(site=="Newbury")

ggplot(gloeo_light_site, aes(x=light_sum,y=totalperL))+
  geom_point(size=2)+
  facet_wrap(~year,scales ="free")+
  labs(title = "Newbury")

ggsave("Datasets/Sunapee/Data Visualizations/Light/Coffin-Gloeo_v_Light-2009_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Fichter-Gloeo_v_Light-2009_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Midge-Gloeo_v_Light-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Newbury-Gloeo_v_Light-2005_2016.pdf",width=15, height=8.5)

# Water temp data ####
watertemp_day <- read_csv("Datasets/Sunapee/SummarizedData/watertemp_day_OnsetData_aggregation.csv", 
                      col_types = cols(date = col_date()))

#convert water temp day to true long format
onset_watertemp_day_long <- onset_watertemp_day %>%
  gather(key = "metric", value = "watertemp", -date) %>% #gather all variables except datetime
  separate(metric,into = c("site","metric")) %>% 
  mutate(year = year(date)) 

#convert water temp day to partial long format to merge with gloeo
onset_watertemp_day_long <- onset_watertemp_day %>%
  gather(key = "metric", value = "watertemp", -date) %>% #gather all variables except datetime
  separate(metric,into = c("site","metric")) %>% 
  spread(key = metric, value = watertemp) %>% 
  arrange(site, date) %>% 
  #filter(obs > 108) %>% 
  rename(watertemp_daily_mean = mean, watertemp_daily_median = median, watertemp_daily_max = max,watertemp_daily_min = min, watertemp_daily_sd = sd,watertemp_daily_obs = obs) %>% 
  mutate(watertemp_daily_range =watertemp_daily_max -watertemp_daily_min)  %>% 
  mutate(watertemp_daily_cv =watertemp_daily_sd/watertemp_daily_mean)

write_csv(onset_watertemp_day_long, "Datasets/Sunapee/SummarizedData/Onset_watertemp_day_long_14Aug2019.csv")

onset_watertemp_day_Midge <- onset_watertemp_day_long %>% 
  filter(site=="midge")

gloeo_Midge_watertemp_day <- full_join(gloeo_Midge,onset_watertemp_day_Midge, by = "date")

write_csv(gloeo_Midge_watertemp_day, "Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp.csv")

#convert water temp week to partial long format to merge with gloeo
onset_watertemp_week_long <- onset_watertemp_week %>%
  gather(key = "metric", value = "watertemp", -c(year,week)) %>% #gather all variables except datetime
  separate(metric,into = c("site","metric")) %>% 
  spread(key = metric, value = watertemp) %>% 
  arrange(site, year, week) %>% 
  #filter(obs > 108) %>% 
  #rename(watertemp_daily_mean = mean, watertemp_daily_median = median, watertemp_daily_max = max,watertemp_daily_min = min, watertemp_daily_sd = sd,watertemp_daily_obs = obs) %>% 
  mutate(watertemp_weekly_range = max-min)  %>% 
  mutate(watertemp_weekly_cv = sd/mean)

write_csv(onset_watertemp_week_long, "Datasets/Sunapee/SummarizedData/Onset_watertemp_week_long_14Aug2019.csv")

onset_watertemp_week_Midge <- onset_watertemp_week_long %>% 
  filter(site=="midge")

gloeo_Midge_watertemp_week <- full_join(gloeo_Midge,onset_watertemp_week_Midge, by = c("year", "week"))

write_csv(gloeo_Midge_watertemp_week, "Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_week.csv")


# Filter by site NOTE: metric field not there anymore, so commented out (IMM)
watertemp_day_long_site <- watertemp_day_long %>% 
  #filter(metric != "obs") %>% 
  #filter(watertemp != "NA") %>% #this field also not there anymore-IMM
  filter(watertemp_mean != 'NA')%>%
  #filter(site=="Coffin")
  #filter(site=="Fichter")
  #filter(site=="Midge")
  filter(site=="Newbury")

# Figure for avg water temp data with site colored and year faceted
ggplot(watertemp_day_long_site, aes(x=date,y=watertemp_mean, color = metric))+
  geom_line(size=1)+
  scale_color_manual(values = c("red","blue","green", "navy"))+
  #geom_line(aes(x=date,y=watertemp_min), size=1.5, color = "navy")+
  #geom_line(aes(x=date,y=watertemp_max), size=1.5, color = "red")+
  facet_wrap(~year,scales ="free_x")+
  labs(title = "Newbury")

ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Coffin-DailyWatertemp_2007-2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Fichter-DailyWatertemp_2007-2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Midge-DailyWatertemp_2006-2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Newbury-DailyWatertemp_2007-2016.pdf",width=15, height=8.5)


#join gloeo, light, & watertemp
gloeo_light_wtr <- left_join(gloeo_light,watertemp_day_long, by = c("date", "site", "year"))

# filter out extra columns
gloeo_light_wtr_clean <- gloeo_light_wtr %>% 
  select(-obs.x,-obs.y,-light_max, -light_median)

write.csv(gloeo_light_wtr_clean, "Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp.csv", row.names = F)

# Figure for gloeo vs. date with color gradient for water temp 
gloeo_light_wtr_site <- gloeo_light_wtr %>% 
  #filter(site=="Coffin")
  #filter(site=="Fichter")
  #filter(site=="Midge")
  filter(site=="Newbury")

ggplot(gloeo_light_wtr_site, aes(x=date,y=totalperL, color = watertemp_mean))+
  geom_point(size=2)+
  facet_wrap(~year,scales ="free")+
  labs(title = "Newbury")

#mean water temp
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Coffin-Gloeo_wtrtemp_mean-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Fichter-Gloeo_wtrtemp_mean-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Midge-Gloeo_wtrtemp_mean-gradient-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Newbury-Gloeo_wtrtemp_mean-gradient-2005_2016.pdf",width=15, height=8.5)

#min water temp
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Coffin-Gloeo_wtrtemp_min-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Fichter-Gloeo_wtrtemp_min-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Midge-Gloeo_wtrtemp_min-gradient-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Newbury-Gloeo_wtrtemp_min-gradient-2005_2016.pdf",width=15, height=8.5)

#max water temp
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Coffin-Gloeo_wtrtemp_max-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Fichter-Gloeo_wtrtemp_max-gradient-2007_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Midge-Gloeo_wtrtemp_max-gradient-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Newbury-Gloeo_wtrtemp_max-gradient-2005_2016.pdf",width=15, height=8.5)

# Figure for gloeo vs. water temp by each year
gloeo_light_wtr_site <- gloeo_light_wtr %>% 
  #filter(site=="Coffin")
  #filter(site=="Fichter")
  filter(site=="Midge")
  #filter(site=="Newbury")

ggplot(gloeo_light_wtr_site, aes(x=watertemp_min,y=totalperL))+
  geom_point(size=2)+
  facet_wrap(~year,scales ="free")+
  labs(title = "Midge")

ggplot(gloeo_light_wtr_site, aes(x=watertemp_min,y=totalperL, color = factor(year)))+
  geom_point(size=2) +
  scale_y_continuous(limits = c(0,20))

ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Coffin-Gloeo_v_wtrtemp-mean-2009_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Fichter-Gloeo_v_wtrtemp-mean-2009_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Midge-Gloeo_v_wtrtemp-mean-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Newbury-Gloeo_v_wtrtemp-mean-2005_2016.pdf",width=15, height=8.5)

ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Coffin-Gloeo_v_wtrtemp-min-2009_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Fichter-Gloeo_v_wtrtemp-min-2009_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Midge-Gloeo_v_wtrtemp-min-2005_2016.pdf",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Water Temp/Newbury-Gloeo_v_wtrtemp-min-2005_2016.pdf",width=15, height=8.5)

# logistic relationship between watertemp and gloeo? (IMM)
# with help from: http://rstudio-pubs-static.s3.amazonaws.com/752_54c50d2916a34e87be430b97c6b5abbe.html
logt <- nls(totalperL ~ SSlogis(watertemp_mean, phi1, phi2, phi3), data = gloeo_light_wtr_site)
summary(logt)
alpha <- coef(logt)  #extracting coefficients
plot(totalperL ~ watertemp_mean, data = gloeo_light_wtr_site, main = "Logistic Model", las=1,
     xlab = "watertemp_mean", ylab = "totalperL", xlim = c(0, 26), ylim = c(0, 20))  # Census data
curve(alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3])), add = T, col = "dodgerblue", lwd=2)  # Fitted model



# PRISM Data ####

# Read in PRISM data @ Midge
prism_midge <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

prism_coffin <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_coffin.csv", skip = 10)

prism_fichter <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_fichter.csv", skip = 10)

prism_newbury <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_newbury.csv", skip = 10)

prism_all <- full_join(prism_coffin, prism_fichter, by = "Date")

prism_all2 <- full_join(prism_all, prism_midge, by = "Date")

prism_all3 <- full_join(prism_all2, prism_newbury, by = "Date")

write_csv(prism_all3, "Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/prism_all3.csv")

#Read in combined data with new headers

prism_all <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_sites_combined.csv")

#convert to long

prism_all_long <- prism_all %>% 
  gather(key = "site", value = "temp", -Date) %>% 
  separate(site, into = c("site","var"), sep = "_") %>% 
  spread(key = var, value = temp) %>% 
  arrange(site, date)

# Subset for 2005-2016
prism_midge_2005 <- prism_midge %>%
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  mutate(dayofmonth = day(Date)) %>% 
  filter(year > 2004 & year < 2017) 

str(prism_midge_2005)

# Join Midge gloeo & Prism
gloeo_Midge_prism <- left_join(gloeo_Midge,prism_midge_2005,by=c("year", "month","dayofmonth"))


# Seasons 
install.packages("zoo")
library(zoo)

water_year_last_month <- 9
date_format <- "%m/%d/%Y"

# Create new columns
prism_all_long <- prism_all_long %>% 
  mutate(CalYear = year(Date)) %>% 
  mutate(Month = month(Date)) 
  
prism_all_long$WaterYear <- NA #create new column for water year

# use conditional statement to fill in water year column
# calendar year is water year for months including and before water_year_last_month, otherwise use 1+calendar year
prism_all_long$WaterYear <- ifelse(prism_all_long$Month <= water_year_last_month, 
                                   prism_all_long$CalYear, prism_all_long$CalYear+1)

# seasons (N Hemisphere): fall: sep-nov, winter: dec-feb, spring: mar-may, summer:jun-aug
prism_all_long$Season <- NA #create new column for season
yq <- as.yearqtr(as.yearmon(prism_all_long$Date, date_format) + 1/12)
prism_all_long$Season <- factor(format(yq, "%q"), levels=1:4,
                                      labels=c('winter','spring','summer','fall'))

prism_2005 <- prism_all_long %>% 
  filter(CalYear > 2004 & CalYear < 2017) %>% 
  filter(site == "midge")

prism_2005 <- prism_all_long %>% 
  filter(CalYear > 2004 & CalYear < 2017) %>% 
  filter(site == "newbury")


# All sites 2005-2016
ggplot(prism_2005, aes(x = Date, y = ppt, color = site)) +
  geom_line(size = 1.5) +
  labs(title = "Daily Precipitation from PRISM", color = "Site", y = "Precipitation (mm)") +
  facet_wrap(~site) +
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("Prism_Precip_AllSites.pdf",width=15, height=8.5)

################ Seasonal weather summaries ##################
# from early 2018 exploratory data analyses
# precip
seasonal_precip <- aggregate(prism_2005$ppt, by=list(prism_2005$WaterYear,prism_2005$Season), FUN=sum, na.rm=T)
colnames(seasonal_precip) <- c('WaterYear','Season','Precip_mm')

seasonal_precip <- seasonal_precip %>% 
  filter(WaterYear != 2017)

ggplot(seasonal_precip, aes(factor(WaterYear), Precip_mm, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Precipitation PRISM Data @ Midge', x='water Year',y='Precipitation (mm)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_Precip_Midge-Season.pdf",width=15, height=8.5)

#Newbury
ggplot(seasonal_precip, aes(factor(WaterYear), Precip_mm, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Precipitation PRISM Data @ Newbury', x='water Year',y='Precipitation (mm)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_Precip_Newbury-Season.pdf",width=15, height=8.5)


# tmin
seasonal_tmin <- aggregate(prism_2005$tmin, by=list(prism_2005$WaterYear,prism_2005$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmin) <- c('WaterYear','Season','tmin_C')

seasonal_tmin <- seasonal_tmin %>% 
  filter(WaterYear != 2017)

#Midge
ggplot(seasonal_tmin, aes(factor(WaterYear), tmin_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Min Air Temp PRISM Data @ Midge', x='water Year',y='Min Air Temp (°C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna')) +
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_MinAirTemp_Midge-Season.pdf",width=15, height=8.5)

#Newbury
ggplot(seasonal_tmin, aes(factor(WaterYear), tmin_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Min Air Temp PRISM Data @ Newbury', x='water Year',y='Min Air Temp (°C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna')) +
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_MinAirTemp_Newbury-Season.pdf",width=15, height=8.5)


# tmax
seasonal_tmax <- aggregate(prism_2005$tmax, by=list(prism_2005$WaterYear,prism_2005$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmax) <- c('WaterYear','Season','tmax_C')
seasonal_tmax <- seasonal_tmax %>% 
  filter(WaterYear != 2017)

#Midge
ggplot(seasonal_tmax, aes(factor(WaterYear), tmax_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Max Air Temp PRISM Data @ Midge', x='water Year',y='Max Air Temp (°C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_MaxAirTemp_Midge-Season.pdf",width=15, height=8.5)

#Newbury
ggplot(seasonal_tmax, aes(factor(WaterYear), tmax_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Max Air Temp PRISM Data @ Newbury', x='water Year',y='Max Air Temp (°C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_MaxAirTemp_Newbury-Season.pdf",width=15, height=8.5)


# tmean
seasonal_tmean <- aggregate(prism_2005$tmean, by=list(prism_2005$WaterYear,prism_2005$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmean) <- c('WaterYear','Season','tmean_C')
seasonal_tmean <- seasonal_tmean %>% 
  filter(WaterYear != 2017)

#Midge
ggplot(seasonal_tmean, aes(factor(WaterYear), tmean_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Mean Air Temp PRISM Data @ Midge', x='water Year',y='Mean Air Temp (°C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_MeanAirTemp_Midge-Season.pdf",width=15, height=8.5)

#Newbury
ggplot(seasonal_tmean, aes(factor(WaterYear), tmean_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Mean Air Temp PRISM Data @ Newbury', x='water Year',y='Mean Air Temp (°C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))

ggsave("Prism_MeanAirTemp_Newbury-Season.pdf",width=15, height=8.5)


# Wind data ####
buoy_wind <- read_csv("Datasets/Sunapee/RawData/Sunapee buoy data/met_data/2007-2017_wind_L1.csv", col_types = cols(
  datetime = col_datetime(format = ""),
  location = col_character(),
  WindDir_deg = col_double(),
  WindSp_ms = col_double(),
  AveWindDir_deg = col_double(),
  AveWindSp_ms = col_double(),
  MaxWindSp_ms = col_double(),
  MaxWindDir_deg = col_double()))

str(buoy_wind)

# Filter dataset for 2007 - 2016

buoy_wind_2007_2016 <- buoy_wind %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(month = month(datetime)) %>% 
  filter(year < 2017)

wind_sp_summary <- buoy_wind_2007_2016 %>% 
  select(date,WindSp_ms, AveWindSp_ms, MaxWindSp_ms) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean, median, min, max, sd), na.rm=T) %>% 
  mutate(WindSp_ms_cv = WindSp_ms_sd/WindSp_ms_mean) %>% 
  mutate(AveWindSp_ms_cv = AveWindSp_ms_sd/AveWindSp_ms_mean) %>% 
  mutate(MaxWindSp_ms_cv = MaxWindSp_ms_sd/MaxWindSp_ms_mean) %>% 
  mutate(year = year(date)) #%>% 
  #mutate(week = week(date))

x <- wind_sp_summary
wind_sp_summary <- replace(x,x == Inf|x == -Inf, NA)

write_csv(wind_sp_summary, "Datasets/Sunapee/SummarizedData/wind_sp_summary.csv")

ggplot(wind_sp_summary, aes(x = week, y = WindSp_ms_mean)) +
  geom_line(size = 1,col="blue")+
  labs(title = "Buoy Avg Daily Wind Speed Instaneous Readings (m/s)",y = "Mean Daily Wind Speed (m/s)") +
  geom_point(aes(x = week, y = AveWindSp_ms_mean), col="red")+
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  #ylim(0,10)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        legend.title = element_text(size=24),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=24,color="black"),
        strip.text = element_text(size=24,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("Buoy_Mean_Daily_WindSpeed.pdf",width=15, height=8.5)
ggsave("Buoy_Mean_Daily_WindSpeed_Instant.pdf",width=15, height=8.5)

# Correlations with log gloeo vs. wind speed ####
gloeo_Midge_watertemp_day <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_day_14Aug2019.csv")

wind_sp_summary <- read_csv("Datasets/Sunapee/SummarizedData/wind_sp_summary.csv", guess_max = 2000)

gloeo_Midge_wind_sp <- left_join(gloeo_Midge_watertemp_day,wind_sp_summary,by=c("date","year"))

write_csv(gloeo_Midge_wind_sp, "Datasets/Sunapee/SummarizedData/gloeo_Midge_windspeed.csv")

summary(gloeo_Midge_wind_sp)

gloeo_Midge_noNA <- gloeo_Midge_wind_sp %>% 
  filter(MaxWindSp_ms_max!="NA")

# Real 1 week lag
gloeo_Midge_1week_lag <- gloeo_Midge_wind_sp %>% 
  mutate(date_1weeklag = date - dweeks(1)) %>% 
  select(date, totalperL, log_gloeo, date_1weeklag)


wind_sp_summary_lag <- wind_sp_summary %>% 
  #filter(site=="midge") %>% 
  rename(date_1weeklag = date)

#Join with 1 week lag date
gloeo_Midge_wind_sp_1week_lag <- left_join(gloeo_Midge_1week_lag, wind_sp_summary_lag,by = "date_1weeklag")

gloeo_Midge_noNA <- gloeo_Midge_wind_sp_1week_lag %>% 
  filter(MaxWindSp_ms_max!="NA") %>% 
  filter(totalperL != 0)

#Function for extracting equation and r2 for linear model (requires plyr library and best if loaded before tidyverse since older version)
lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ MaxWindSp_ms_max, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$MaxWindSp_ms_max, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels

ggplot(gloeo_Midge_noNA, aes(x=MaxWindSp_ms_max,y=log_gloeo))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 week Lag Max Daily Max Wind Speed",color="Month",title = "Midge")+
  #xlim(19,27)+
  #ylim(19,27)+
  geom_text(x = 4, y=2, aes(label = formula), data = labels, parse = T, hjust = 0)+
  geom_text(x = 5, y=1.5, aes(label = r2), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 19, y= 22.5, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 19, y= 22, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_InstWindSp_Mean.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_InstWindSp_Median.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_InstWindSp_CV.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_AveWindSp_Mean.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_AveWindSp_Min.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_AveWindSp_CV.pdf", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_LagMaxWindSp_Max.pdf", width = 11, height = 8.5)


# Wind direction ####
#wind direction is reported by direction in which it originates - i.e. westerly wind blows from west to east
# 0 or 360 = N, 90 = E, 180 = S, 270 = W

wind_direction_summary <- buoy_wind_2007_2016 %>% 
  select(date,WindDir_deg, AveWindDir_deg, MaxWindDir_deg) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean, median, min, max, sd), na.rm=T) %>% 
  #mutate(WindSp_ms_cv = WindSp_ms_sd/WindSp_ms_mean) %>% 
  #mutate(AveWindSp_ms_cv = AveWindSp_ms_sd/AveWindSp_ms_mean) %>% 
  #mutate(MaxWindSp_ms_cv = MaxWindSp_ms_sd/MaxWindSp_ms_mean) %>% 
  mutate(year = year(date)) #%>% 
#mutate(week = week(date))

x <- wind_direction_summary
wind_direction_summary <- replace(x,x == Inf|x == -Inf, NA)

write_csv(wind_direction_summary, "Datasets/Sunapee/SummarizedData/wind_direction_summary.csv")

# Wind direction packages
install.packages("openair")
library(openair)

#Instantaneous wind dir & speed
inst_buoy_wind_2007_2016_noNA <- buoy_wind_2007_2016 %>% 
  filter(WindSp_ms !="NA") %>% 
  filter(WindDir_deg!="NA") %>% 
  filter(month %in% c(5,6,7,8,9,10))

summary(inst_buoy_wind_2007_2016_noNA)
which.max(inst_buoy_wind_2007_2016_noNA$WindSp_ms)  #Feb 2011 crazy high value

windRose(inst_buoy_wind_2007_2016_noNA, ws = "WindSp_ms", wd = "WindDir_deg", type = "year",paddle = F , breaks = c(0,2,4,6,8,10))

windRose(inst_buoy_wind_2007_2016_noNA, ws = "WindSp_ms", wd = "WindDir_deg", type = "month",paddle = F , breaks = c(0,2,4,6,8,10))

quartz.save("~/Desktop/Gloeo Plots/Sunapee_Inst_WindDir_Summer_2009-2014.jpeg",type="jpeg", width=11, height=8.5)

#Ave wind dir & speed
ave_buoy_wind_2007_2016_noNA <- buoy_wind_2007_2016 %>% 
  filter(AveWindSp_ms !="NA") %>% 
  filter(AveWindDir_deg!="NA") %>% 
  filter(month %in% c(5,6,7,8,9,10))

#different breaks for wind speed by year
windRose(ave_buoy_wind_2007_2016_noNA, ws = "AveWindSp_ms", wd = "AveWindDir_deg", breaks = c(0,2,4,6,8,10), type = "year",paddle = T, width = 2)
quartz.save("~/Desktop/Gloeo Plots/Sunapee_Ave_WindDir_Summer_2009-2016.jpeg",type="jpeg", width=11, height=8.5)

# by month
windRose(ave_buoy_wind_2007_2016_noNA, ws = "AveWindSp_ms", wd = "AveWindDir_deg", breaks = c(0,2,4,6,8,10), type = "month",paddle = F) #T, width = 2)

quartz.save("~/Desktop/Gloeo Plots/Sunapee_Ave_WindDir_Summer_2009-2016-month.jpeg",type="jpeg", width=11, height=8.5)

# Filter for specific year - 2013, big bloom
buoy_wind_2013_noNA <- buoy_wind_2007_2016 %>% 
  #filter(WindSp_ms !="NA") %>% 
  #filter(WindDir_deg!="NA") %>% 
  mutate(day = day(datetime)) %>% 
  filter(location=="loon") %>% 
  filter(month %in% c(5,6,7,8,9,10)) %>% 
  filter(year == 2013) %>% 
  filter(month==9)

days <- c(1:30)
buoy_wind_2013_noNA$day <- as.factor(buoy_wind_2013_noNA$day)
str(buoy_wind_2013_noNA)

sum(is.na(buoy_wind_2013_noNA))

windRose(buoy_wind_2013_noNA, ws = "AveWindSp_ms", wd = "AveWindDir_deg", breaks = c(0,2,4,6,8,10), type = "day",paddle = T, width = 2)

quartz.save("~/Desktop/Gloeo Plots/Sunapee_Ave_WindDir_2013-day.jpeg",type="jpeg", width=11, height=8.5)


# Data exploration for light vs. total & daily diff gloeo ####
gloeo_site <- gloeo %>%
  filter(site=="Midge") %>% 
  filter(year > 2008) %>% 
  filter(totalperL > 0)

str(gloeo_site)

ggplot(gloeo_site, aes(x=light_sum,y=totalperL, color=factor(year)))+
  geom_point(size=2)+
  facet_wrap(~year,scales ="free")+
  labs(title = "Midge", color = "Year")

ggsave("Datasets/Sunapee/Data Visualizations/Light/Midge_total_light.jpeg",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Midge_diff_light.jpeg",width=15, height=8.5)

ggsave("Datasets/Sunapee/Data Visualizations/Light/Newbury_total_light.jpeg",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Newbury_diff_light.jpeg",width=15, height=8.5)

ggsave("Datasets/Sunapee/Data Visualizations/Light/Coffin_total_light.jpeg",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Coffin_diff_light.jpeg",width=15, height=8.5)

ggsave("Datasets/Sunapee/Data Visualizations/Light/Fichter_total_light.jpeg",width=15, height=8.5)
ggsave("Datasets/Sunapee/Data Visualizations/Light/Fichter_diff_light.jpeg",width=15, height=8.5)

# Wind vs. gloeo

gloeo_wind <- full_join(gloeo,wind_sp_summary, by = "date")

gloeo_site <- gloeo %>%
  filter(site=="Midge") %>% 
  #filter(year > 2008) %>% 
  filter(totalperL > 0)

str(gloeo_site)

ggplot(gloeo_site, aes(x=AveWindSp_ms_mean,y=totalperL, color=factor(year)))+
  geom_point(size=2)+
  facet_wrap(~year,scales ="free")+
  labs(title = "Midge", color = "Year")

hist(gloeo$totalperL)
hist(gloeo_site$totalperL)



# Read in buoy water temp data for regressions with Onset Data ####
setwd("~/Google Drive/GLEON_Bayesian_WG/Sunapee_buoydata/tempstring_data")
buoy_water_temp <- read_csv("2007-2017_fulltemprecord_L1-JB.csv")
onset_water_temp <- read_csv("Datasets/Sunapee/SummarizedData/Onset_wtrtemp_60min_2006-2016_Allsites.csv",col_types = cols(
  coffin = col_double(),
  fichter = col_double(),
  newbury = col_double()))

onset_day_summary <- read_csv("Datasets/Sunapee/SummarizedData/watertemp_day_OnsetData_aggregation.csv")


gloeo_wtr <- read_csv("all_data_NA.csv", col_types = cols(
  site = col_factor(levels = c("coffin","fichter","midge","newbury")),
  date = col_date(format = ""),
  year = col_double(),
  month = col_double(),
  dayofmonth = col_double(),
  dayofyr = col_double(),
  coloniesperL = col_double(),
  filbundperL = col_double(),
  totalperL = col_double(),
  totalperL_diff = col_double(),
  week = col_double(),
  light_sum = col_double(),
  watertemp_mean = col_double(),
  watertemp_min = col_double(),
  watertemp_max = col_double(),
  watertemp_median = col_double(),
  ysi_readings = col_double(),
  buoy_interp = col_double(),
  daylength = col_double()))

str(gloeo_wtr)


buoy <- read_csv("buoy_daily.csv", col_types = cols(
  site = col_factor(levels = c("coffin","fichter","midge","newbury")),
  date = col_date(format = ""),
  watertemp_mean = col_double(),
  watertemp_min = col_double(),
  watertemp_max = col_double(),
  watertemp_median = col_double()))

#Convert onset data to long with site column
onset_water_temp_long <- onset_water_temp %>% 
  gather(key="site",value="onset_wtr_temp",c(coffin,fichter,newbury,midge))

#Midge subset 2005-2017
#Year = 2008
#Month = 

onset_day_summary <- onset_water_temp_long %>% 
  mutate(month = month(datetime)) %>%
  mutate(day = day(datetime)) %>% 
  mutate(hour = hour(datetime)) %>% 
  group_by(year,month,day,hour) %>% 
  summarize_all(funs(mean,min,max,median), na.rm = T)

#2008:June 17 13:00 - Sep 16 23:00
#2010: May 27 14:00 - sep 22 9:00
#2011: June 2 7:00 - Sep 22 8:00
#2012: May 24 13:00 - Sep 27 8:00


buoy <- buoy_water_temp %>% 
  mutate(year = year(datetime)) %>% 
  mutate(month = month(datetime)) %>%
  mutate(day = day(datetime)) %>% 
  mutate(hour = hour(datetime)) %>% 
  #filter(location=="loon") %>% 
  filter(year == 2016) #%>% 
  #filter(month %in% c(3,4,5,6,7,8,9,10))

sum(is.na(buoy$TempC_1p5m))

#Hourly summary of buoy water temp at 0.5 m, 1 m, 1.5 m
buoy_hourly <- buoy %>% 
  select(year,month,day,hour,TempC_1p5m) %>% 
  group_by(year,month,day,hour) %>% 
  summarize_all(funs(mean, median), na.rm = T)
  #filter(datetime_mean > "2008-06-17 08:25:00") %>% 
  #filter(datetime_mean < "2009-09-21 06:25:00")
  #filter(datetime_mean > "2010-05-27 09:25:00") %>% 
  #filter(datetime_mean < "2010-09-22 06:25:00")
  #filter(datetime_mean > "2011-06-02 03:25:00") %>% 
  #filter(datetime_mean < "2011-09-22 05:25:00")
  #filter(datetime_mean > "2012-05-24 08:25:00") %>% 
  #filter(datetime_mean < "2012-09-27 05:25:00")

buoy_daily <- buoy %>% 
  select(year,month,day,TempC_1p5m) %>% 
  group_by(year,month,day) %>% 
  summarize_all(funs(mean,min,max,median), na.rm = T)

buoy_daily_final <- replace(buoy_daily,buoy_daily == Inf|buoy_daily == -Inf, NA)

write.csv(buoy_daily_final, "~/Desktop/buoy_daily_final.csv",row.names = F)

# Join buoy and onset datasets
buoy_onset_join <- left_join(buoy_hourly,onset,by=c("year", "month","day", "hour"))
onset_buoy_all <- left_join(onset,buoy_hourly,by=c("year", "month","day", "hour"))

onset_buoy <- onset_buoy_all %>% 
  select(-c(datetime_mean,datetime_median)) %>% 
  #select(-c(datetime_mean,datetime_median,TempC_1p5m_mean,TempC_1p5m_median)) %>% 
  #filter(site=="coffin") %>% 
  filter(month == 9)

#2008 remove NA
onset_buoy <- na.omit(onset_buoy)

fit1 <- lm(onset_wtr_temp ~ TempC_1p5m_mean, data = onset_buoy_filter)
summary(fit1)
#equation: onset = buoy*0.961 + 0.95, r2=0.879 #all months
#equation: onset = buoy*0.853 + 2.97, r2=0.405 #just June

# Linear model function to text annotation ####
lm_labels <- function(dat) {
  mod <- lm(onset_wtr_temp ~ TempC_1p5m_mean, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$TempC_1p5m_mean, dat$onset_wtr_temp)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = F)
}

library(plyr)
labels <- ddply(onset_buoy, "site", lm_labels)
labels

ggplot(onset_buoy, aes(x=TempC_1p5m_mean,y=onset_wtr_temp))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  labs(y="Onset Logger Water Temp (°C)", x="Hourly Mean Buoy 1.5 m Water Temp (°C)",color="Month",title = "May 2012")+
  #xlim(19,27)+
  #ylim(19,27)+
  #geom_text(x = 20.5, y=17.5, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 20.5, y=17, aes(label = r2), data = labels, parse = T, hjust = 0)+
  geom_text(x = 22, y= 19, aes(label = formula), data = labels, parse = T, hjust = 0)+
  geom_text(x = 22, y= 18.5, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~site)

ggsave("All_June_2008.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2008.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2009.pdf", width = 11, height = 8.5)
ggsave("All_May_2010.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2010.pdf", width = 11, height = 8.5)
ggsave("All_June_2011.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2011.pdf", width = 11, height = 8.5)
ggsave("All_May_2012.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2012.pdf", width = 11, height = 8.5)
ggsave("All_May_2013.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2013.pdf", width = 11, height = 8.5)
ggsave("All_June_2014.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2014.pdf", width = 11, height = 8.5)
ggsave("All_June_2015.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2015.pdf", width = 11, height = 8.5)
ggsave("All_June_2016.pdf", width = 11, height = 8.5)
ggsave("All_Sep_2016.pdf", width = 11, height = 8.5)
ggsave("All_June-Sep_2016.pdf", width = 11, height = 8.5)

# Test interp with June vs. all months ####
onset_buoy_filter_interp <- buoy_onset_join %>% 
  mutate(coffin_onset_interp =  -0.66 +1.03*TempC_1p5m_mean) %>% 
  mutate(fichter_onset_interp = 0.35 +0.98 *TempC_1p5m_mean) %>% 
  mutate(midge_onset_interp = 0.30 +1.00 *TempC_1p5m_mean) %>% 
  mutate(newbury_onset_interp = -1.46 +1.07 *TempC_1p5m_mean) %>% 
  select(year,month,day,coffin_onset_interp,fichter_onset_interp,midge_onset_interp,newbury_onset_interp) %>%
  group_by(year,month,day) %>% 
  summarize_all(funs(mean,min,max,median), na.rm = T) %>% 
  select(year,month,day,starts_with("coffin"),starts_with("fichter"),starts_with("midge"),starts_with("newbury")) %>% 
  filter(month==10 & day %in% c(5))

# Schmidt and Gloeo correlations ####

# Read in schmidt data
sunapee_schmidt_stability <- readRDS("~/Documents/Gloeo Bayesian Modeling/GLEON_Bayesian_WG/Datasets/Sunapee/Stability_metrics/sunapee_schmidt_stability.rds")

sunapee_schmidt_stability_2015_summary <- sunapee_schmidt_stability[-c(1:6),] %>% 
  mutate(date = date(datetime)) %>% 
  mutate(year = year(datetime)) %>% 
  filter(year==2017) %>% 
  select(-year) %>% 
  #mutate(month = month(datetime)) %>% 
  #mutate(day = day(datetime)) %>% 
  #mutate(hour = hour(datetime)) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean, max,min, median, sd), na.rm = T) %>% 
  select(-c(starts_with("datetime_")))

sunapee_schmidt_stability_2015_summary <- replace(sunapee_schmidt_stability_daily_summary,sunapee_schmidt_stability_2015_summary == Inf|sunapee_schmidt_stability_daily_summary == -Inf, NA)

write_csv(sunapee_schmidt_stability_2015_summary, "~/Desktop/sunapee_schmidt_stability_2015_summary.csv") #Fix Sep 6th & 15 min data holes


#drop days with less than 75% of the data = 144 obs/day * 0.75 = 108 for 10 min data, 15 min data in 2014-2016,  -----
sumfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...),max=max(x, na.rm=TRUE, ...), min=min(x, na.rm=TRUE, ...), 
    median=median(x, na.rm=TRUE, ...), sd=sd(x, na.rm=TRUE, ...), obs=sum(!is.na(x)))}
sunapee_schmidt_stability.df  <- as.data.frame(sunapee_schmidt_stability_daily_summary)
str(sunapee_schmidt_stability.df)

schmidt_day <- summaryBy(schmidt.stability ~ date, data=sunapee_schmidt_stability.df, FUN=sumfun)

schmidt_day_v2 <- replace(schmidt_day,schmidt_day == Inf|schmidt_day == -Inf, NA)

ix=which(schmidt_day_v2$schmidt.stability.obs <108)
for (i in c('schmidt.stability.mean', 'schmidt.stability.max', 'schmidt.stability.min', 'schmidt.stability.median', 'schmidt.stability.sd'))
  {schmidt_day_v2[ix,i]=NA}

sum(is.na(sunapee_schmidt_stability_daily_summary))

gloeo_day <- gloeo %>% 
  mutate(day = day(date))

gloeo_schmidt <- left_join(gloeo,schmidt_day_v2,by=c("date"))

gloeo_schmidt_Midge <- gloeo_schmidt %>% 
  filter(site=="Midge") %>% 
  #filter(year>2006) %>% 
  #filter(month==8) %>% 
  mutate(log_gloeo = log10(totalperL+.0036)) %>% 
  mutate(schmidt.stability_range = schmidt.stability.max - schmidt.stability.min) %>% 
  mutate(schmidt.stability_CV = schmidt.stability.sd/schmidt.stability.mean) 

write_csv(gloeo_schmidt_Midge, "~/Desktop/gloeo_schmidt_Midge_v2.csv") #Fix Sep 6th & 15 min data holes

write_csv(schmidt_day_v2, "~/Desktop/schmidt_day_v2.csv")

gloeo_schmidt_Midge <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_schmidt_Midge_14Aug2019.csv")

gloeo_schmidt_Midge_1weeklag <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_schmidt_Midge_final-1weeklag.csv")

str(gloeo_schmidt_Midge_1weeklag)

# calculate week to week difference in schmidt ####
gloeo_schmidt_Midge_subset <- gloeo_schmidt_Midge %>% 
  select(date,log_gloeo,starts_with("schmidt")) %>% 
  filter(schmidt.stability.max!="NA")
  
write_csv(gloeo_schmidt_Midge_subset, "~/Desktop/gloeo_schmidt_Midge_subset.csv")
gloeo_schmidt_Midge_subset <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_schmidt_Midge_subset.csv")
  
schmidt_mean_diff <- vector("double", nrow(gloeo_schmidt_Midge_subset))  # 1. output
schmidt_max_diff <- vector("double", nrow(gloeo_schmidt_Midge_subset))  # 1. output
schmidt_min_diff <- vector("double", nrow(gloeo_schmidt_Midge_subset))  # 1. output

for (i in 1:nrow(gloeo_schmidt_Midge_subset)) {
  schmidt_mean_diff[i+1] <- gloeo_schmidt_Midge_subset$schmidt.stability.mean[i+1] - gloeo_schmidt_Midge_subset$schmidt.stability.mean[i]
  schmidt_max_diff[i+1] <- gloeo_schmidt_Midge_subset$schmidt.stability.max[i+1] - gloeo_schmidt_Midge_subset$schmidt.stability.max[i]
  schmidt_min_diff[i+1] <- gloeo_schmidt_Midge_subset$schmidt.stability.min[i+1] - gloeo_schmidt_Midge_subset$schmidt.stability.min[i]
  schmidt_output <- data.frame(schmidt.stability_mean_diff = schmidt_mean_diff, schmidt.stability_max_diff = schmidt_max_diff, schmidt.stability_min_diff = schmidt_min_diff)
}

write_csv(schmidt_output, "~/Desktop/schmidt_output.csv")

# schmidt plots ####
gloeo_schmidt_Midge_noNA <- gloeo_schmidt_Midge_1weeklag %>% 
  filter(schmidt.stability.max!="NA")

gloeo_schmidt_Midge_noNA <- gloeo_schmidt_Midge_subset %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  filter(schmidt.stability_max_diff!="NA")
  
str(gloeo_schmidt_Midge_noNA)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ schmidt.stability_CV, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$schmidt.stability_CV)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = F)
}

library(plyr)
labels <- ddply(gloeo_schmidt_Midge_noNA, "year", lm_labels)
labels


ggplot(gloeo_schmidt_Midge_noNA, aes(x=schmidt.stability_range,y=log_gloeo))+ #,color=month))+ #,color=factor(month)))+ #Added 1/2 *detection limit (1/140) to log
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 week LAG RANGE Schmidt Stability",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(-1,1)+
  #ylim(19,27)+
  geom_text(x = 300, y=-2.75, aes(label = formula), data = labels, parse = T, hjust = 0)+
  geom_text(x = 400, y=-2, aes(label = r2), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 19, y= 22.5, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 19, y= 22, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_MaxSchmidt_Midge-month.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MinSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_RangeSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MeanSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_SDSchmidt_Midge.jpeg", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Diff_MaxSchmidt_Midge-month.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Diff_MinSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Diff_MeanSchmidt_Midge.jpeg", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_MaxSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MinSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MeanSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)


data_cal_month <- data %>%
  mutate(month=month(date)) %>% 
  mutate(dayofmonth=day(date)) %>% 
  mutate(week=week(date)) %>% 
  mutate(dayofyr = yday(date))

View(data_cal_month)

write.csv(data_cal_month,"~/Desktop/all_data_NA.csv")

# Test for water temp lags ####

# Final Midge gloeo + water temp data with SD and onset interp added
gloeo_Midge_watertemp_day <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_day_14Aug2019.csv")

gloeo_Midge_watertemp_day_season <- gloeo_Midge_watertemp_day %>% 
  filter(20 < week, week < 41)
  
  
min_watertemp <- gloeo_Midge_watertemp_day %>% 
  filter(20 < week, week < 41) %>% 
  group_by(year) %>% 
  summarize(min_temp = min(watertemp_daily_min, na.rm = T))

min_watertemp_interp <- gloeo_Midge_watertemp_day %>%
  filter(20 < week, week < 41) %>% 
  group_by(year) %>% 
  summarize(min_temp = min(watertemp_min_interp, na.rm = T))

#1 week lag
gloeo_Midge_1week = read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_day-1weeklag_14Aug2019.csv")

# Real 1 week lag
gloeo_Midge_1week_lag <- gloeo_Midge_watertemp_day %>% 
  mutate(date_1weeklag = date - dweeks(1)) %>% 
  select(date, log_gloeo, date_1weeklag)

onset_watertemp_day <-  read_csv("Datasets/Sunapee/SummarizedData/Onset_watertemp_day_long_14Aug2019.csv")

onset_watertemp_day_Midge <- onset_watertemp_day %>% 
  filter(site=="midge") %>% 
  rename(date_1weeklag = date)

#Join water temp with 1 week lag date
join_gloeo_Midge_1week_lag <- left_join(gloeo_Midge_1week_lag, onset_watertemp_day_Midge,by = "date_1weeklag")

write_csv(join_gloeo_Midge_1week_lag, "Datasets/Sunapee/SummarizedData/join_gloeo_Midge_1week_lag-real.csv")


join_gloeo_Midge_1week_lag_yr <- join_gloeo_Midge_1week_lag %>% 
  mutate(year = year(date))

# Weekly summary water temp data ###
gloeo_Midge_watertemp_week <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_week.csv")

# filter out weeks with less than 75% obs, 126
gloeo_Midge_watertemp_week <- gloeo_Midge_watertemp_week %>% 
  filter(watertemp_weekly_obs > 125)


# calculate week to week difference in water temp - mean,min, max ####
wtr_mean_diff <- vector("double", nrow(gloeo_Midge_watertemp_day))  # 1. output
wtr_median_diff <- vector("double", nrow(gloeo_Midge_watertemp_day))  # 1. output
wtr_max_diff <- vector("double", nrow(gloeo_Midge_watertemp_day))  # 1. output
wtr_min_diff <- vector("double", nrow(gloeo_Midge_watertemp_day))  # 1. output
wtr_sd_diff <- vector("double", nrow(gloeo_Midge_watertemp_day))  # 1. output
wtr_range_diff <- vector("double", nrow(gloeo_Midge_watertemp_day))  # 1. output

for (i in 1:nrow(gloeo_Midge_watertemp_day)) {
  wtr_mean_diff[i+1] <- gloeo_Midge_watertemp_day$watertemp_daily_mean[i+1] - gloeo_Midge_watertemp_day$watertemp_daily_mean[i]
  wtr_median_diff[i+1] <- gloeo_Midge_watertemp_day$watertemp_daily_median[i+1] - gloeo_Midge_watertemp_day$watertemp_daily_median[i]
  wtr_max_diff[i+1] <- gloeo_Midge_watertemp_day$watertemp_daily_max[i+1] - gloeo_Midge_watertemp_day$watertemp_daily_max[i]
  wtr_min_diff[i+1] <- gloeo_Midge_watertemp_day$watertemp_daily_min[i+1] - gloeo_Midge_watertemp_day$watertemp_daily_min[i]
  wtr_sd_diff[i+1] <- gloeo_Midge_watertemp_day$watertemp_daily_sd[i+1] - gloeo_Midge_watertemp_day$watertemp_daily_sd[i]
  wtr_range_diff[i+1] <- gloeo_Midge_watertemp_day$watertemp_daily_range[i+1] - gloeo_Midge_watertemp_day$watertemp_daily_range[i]
  wtr_diff_output <- data.frame(date = gloeo_Midge_watertemp_day$date,log_gloeo = gloeo_Midge_watertemp_day$log_gloeo, wtr_mean_diff = wtr_mean_diff[-230], wtr_median_diff = wtr_median_diff[-230], wtr_max_diff = wtr_max_diff[-230], wtr_min_diff = wtr_min_diff[-230], wtr_sd_diff = wtr_sd_diff[-230], wtr_range_diff = wtr_range_diff[-230])
}

write_csv(wtr_diff_output, "~/Desktop/wtr_diff_output.csv")
wtr_diff_output = read_csv("Datasets/Sunapee/SummarizedData/wtr_diff_output.csv")

# Water Temp Plots ####
#Remove missing values for lm function
gloeo_Midge_noNA <- gloeo_Midge_watertemp_day %>% 
  filter(watertemp_daily_min!="NA")

gloeo_Midge_noNA <- gloeo_Midge_1week %>% 
  filter(watertemp_daily_sd!="NA")

gloeo_Midge_noNA <- wtr_diff_output %>% 
  mutate(year = year(date)) %>% 
  filter(wtr_mean_diff!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ watertemp_daily_sd, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$watertemp_daily_sd)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels

ggplot(gloeo_Midge_noNA, aes(x=watertemp_daily_sd,y=log_gloeo))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Weekly SD Water Temp",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = 20, y=-2, aes(label = formula), data = labels, parse = T, hjust = 0)+
  geom_text(x = 20, y=-2.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

# Daily Plots
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_Wtr_Temp.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Median_Wtr_Temp.pdf", width = 11, height = 8.5) #worse than mean
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_Wtr_Temp-month.pdf", width = 11, height = 8.5)#best
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_Wtr_Temp.pdf", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_Wtr_Temp-1week-lag_real.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_Wtr_Temp-1week-lag-real.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_Wtr_Temp-1week-lag.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_SD_Wtr_Temp-1week-lag.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Range_Wtr_Temp-1week-lag.pdf", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_Wtr_Temp-1week-Diff.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_Wtr_Temp-1week-Diff.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_Wtr_Temp-1week-Diff.pdf", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Sum_PAR.pdf", width = 11, height = 8.5)

#weekly plots
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_Wtr_Temp-week.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Median_Wtr_Temp-week.pdf", width = 11, height = 8.5) #worse than mean
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_Wtr_Temp-week.pdf", width = 11, height = 8.5)#best
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_Wtr_Temp-week.pdf", width = 11, height = 8.5)



# Air temp vs. gloeo ####

prism_midge <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

# Subset for 2005-2016
prism_midge_2005 <- prism_midge %>% #[1:13514,]
  mutate(year = year(Date)) %>% 
  #mutate(month = month(Date)) %>% 
  #mutate(dayofmonth = day(Date)) %>%
  rename(date = Date) %>% 
  filter(year > 2004 & year < 2017) 

gloeo_Midge_prism <- left_join(gloeo_Midge_watertemp_day,prism_midge_2005,by=c("date","year"))
write_csv(gloeo_Midge_prism, "Datasets/Sunapee/SummarizedData/gloeo_Midge_airtemp_precip.csv")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ airtemp_max_degrees_C, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$airtemp_max_degrees_C, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = F)
}

sp <- cor(gloeo_Midge_prism$airtemp_max_degrees_C, gloeo_Midge_prism$log_gloeo, method = "pearson")
sp
sp^2

labels <- ddply(gloeo_Midge_prism, "year", lm_labels)
labels

ggplot(gloeo_Midge_prism, aes(x=airtemp_max_degrees_C,y=log_gloeo))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Max Air Temp (°C)",color="Month",title = "Midge")+
  #xlim(19,27)+
  #ylim(19,27)+
  geom_text(x = 20, y=-2.75, aes(label = formula), data = labels, parse = T, hjust = 0)+
  geom_text(x = 25, y=-3.25, aes(label = r2), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 19, y= 22.5, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 19, y= 22, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_Air_Temp.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_Air_Temp.pdf", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_Air_Temp.pdf", width = 11, height = 8.5)
