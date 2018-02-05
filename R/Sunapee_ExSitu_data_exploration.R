############################ Lake Sunapee ex situ data exploration #############################
# Date: 1-24-18
# updated:1-24-18
# JAB check 2 Feb 2018
# Authors: 
################################################################################################

#### R libraries ####
library(lubridate)
library(zoo)
library(ggplot2)
library(tidyverse) #Loads all tidy verse packages
#If you want to load dplyr or tidyr in addition to ggplot2 and lubridate

#### input data ####
setwd("C:/Users/FWL/Documents/GLEON_Bayesian_WG") #Ian working directory
weather_summary_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/weather/Newport2005-2016_summary.csv"))
weather_raw_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/weather/Newport2005-2016_raw.csv"))
midge_gloeo_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/measured in-situ data/Midge_gloeo_2006_2016.csv"))
newport_gloeo_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/measured in-situ data/Newport_gloeo_2006_2016.csv"))

#### define constants ####
water_year_last_month <- 9 #e.g., 9=September (Sep 2005 is the 2005 water year, but Oct 2005 is 2006 water year)
date_format <- "%m/%d/%Y"

#### define functions ####


########################## Main program ##############################
################ data wrangling ###############
# convert date column to date
weather_summary_data$date <- as.Date(weather_summary_data$date, format=date_format)

# create monthly, annual, seasonal columns
weather_summary_data$Month <- month(weather_summary_data$date) #month column
weather_summary_data$CalYear <- year(weather_summary_data$date) #calendar year column
weather_summary_data$WaterYear <- NA #create new column for water year
# use conditional statement to fill in water year column
# use calendar year as water year for months including and before water_year_last_month, otherwise use 1+calendar year
weather_summary_data$WaterYear <- ifelse(weather_summary_data$Month <= water_year_last_month, 
                                         weather_summary_data$CalYear, weather_summary_data$CalYear+1)
# seasons: fall: sep-nov, winter: dec-feb, spring: mar-may, summer:jun-aug
weather_summary_data$Season <- NA #create new column for season
yq <- as.yearqtr(as.yearmon(weather_summary_data$date, "%m/%d/%Y") + 1/12)
weather_summary_data$Season <- factor(format(yq, "%q"), levels=1:4,
                                      labels=c('winter','spring','summer','fall'))

############ Annual climate summaries ##############
# may not be very useful, but gives sense of "wet" or "warm" years

# precip
WY_precip <- aggregate(weather_summary_data$daily_precip_mm, by=list(weather_summary_data$WaterYear),
                                                                     FUN=sum, na.rm=T)
colnames(WY_precip) <- c('WaterYear','daily_precip_mm')
plot(daily_precip_mm ~ WaterYear, WY_precip, type='l', las=1, lwd=2, col='black',
     main='Total water-year precipitation')
mtext('Newport Station', side=3)

# tmin
WY_tmin <- aggregate(weather_summary_data$min_dailytemp_C, by=list(weather_summary_data$WaterYear),
                     FUN=mean, na.rm=T)
colnames(WY_tmin) <- c('WaterYear','min_dailytemp_C')
plot(min_dailytemp_C ~ WaterYear, WY_tmin, type='l', las=1, lwd=2, col='dodgerblue',
     main='Mean annual minimum air temperature')
mtext('Newport Station', side=3)

# tmax
WY_tmax <- aggregate(weather_summary_data$max_dailytemp_C, by=list(weather_summary_data$WaterYear),
                     FUN=mean, na.rm=T)
colnames(WY_tmax) <- c('WaterYear','max_dailytemp_C')
plot(max_dailytemp_C ~ WaterYear, WY_tmax, type='l', las=1, lwd=2, col='firebrick',
     main='Mean annual maximum air temperature')
mtext('Newport Station', side=3)

# snowfall
WY_snowfall <- aggregate(weather_summary_data$daily_snowfall_mm, by=list(weather_summary_data$WaterYear),
                         FUN=sum, na.rm=T)
colnames(WY_snowfall) <- c('WaterYear','daily_snowfall_mm')
plot(daily_snowfall_mm ~ WaterYear, WY_snowfall, type='l', las=1, lwd=2, col='black',
     main='Total water-year snowfall')
mtext('Newport Station', side=3)


################ Seasonal climate summaries ##################
# precip
seasonal_precip <- aggregate(weather_summary_data$daily_precip_mm, by=list(weather_summary_data$WaterYear,
                                                                           weather_summary_data$Season), FUN=sum, na.rm=T)
colnames(seasonal_precip) <- c('WaterYear','Season','Precip_mm')

ggplot(seasonal_precip, aes(factor(WaterYear), Precip_mm, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Newport Station', x='water Year',y='Precipitation (mm)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))

# tmin
seasonal_tmin <- aggregate(weather_summary_data$min_dailytemp_C, by=list(weather_summary_data$WaterYear,
                                                                         weather_summary_data$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmin) <- c('WaterYear','Season','tmin_C')

ggplot(seasonal_tmin, aes(factor(WaterYear), tmin_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Newport Station', x='water Year',y='tmin (C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))

# tmax
seasonal_tmax <- aggregate(weather_summary_data$max_dailytemp_C, by=list(weather_summary_data$WaterYear,
                                                                         weather_summary_data$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmax) <- c('WaterYear','Season','tmax_C')

ggplot(seasonal_tmax, aes(factor(WaterYear), tmax_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Newport Station', x='water Year',y='tmax (C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))

#This is Mary Lofton making a test change!