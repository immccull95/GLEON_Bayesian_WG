############################ Lake Sunapee ex situ data exploration #############################
# Date: 1-24-18
# updated:5-31-18
# Authors: Ian McCullough 
################################################################################################

#### R libraries ####
library(lubridate)
library(zoo)
library(ggplot2)
library(tidyverse) #Loads all tidy verse packages

#### set working directory ####
#please comment out and make new line for your own directory
setwd("C:/Users/FWL/Documents/GLEON_Bayesian_WG") #Ian's working directory

#### input data ####
weather_summary_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/Raw Data/weather/Newport2005-2016_summary.csv"))
weather_raw_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/Raw Data/weather/Newport2005-2016_raw.csv"))
#midge_gloeo_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/Raw Data/measured in-situ data/Midge_gloeo_2006_2016.csv"))
#newport_gloeo_data <- read.csv(paste0(getwd(),"/Datasets/Sunapee/Raw Data/measured in-situ data/Newport_gloeo_2006_2016.csv"))

#### define constants ####
water_year_last_month <- 9 #e.g., 9=September (Sep 2005 is the 2005 water year, but Oct 2005 is 2006 water year)
date_format <- "%m/%d/%Y"

########################## Main program ##############################
################ data wrangling ###############
# convert date column to date
weather_summary_data$date <- as.Date(weather_summary_data$date, format=date_format)

# create monthly, annual, seasonal columns
weather_summary_data$Month <- month(weather_summary_data$date) #month column
weather_summary_data$CalYear <- year(weather_summary_data$date) #calendar year column
weather_summary_data$WaterYear <- NA #create new column for water year

# use conditional statement to fill in water year column
# calendar year is water year for months including and before water_year_last_month, otherwise use 1+calendar year
weather_summary_data$WaterYear <- ifelse(weather_summary_data$Month <= water_year_last_month, 
                                         weather_summary_data$CalYear, weather_summary_data$CalYear+1)
# seasons (N Hemisphere): fall: sep-nov, winter: dec-feb, spring: mar-may, summer:jun-aug
weather_summary_data$Season <- NA #create new column for season
yq <- as.yearqtr(as.yearmon(weather_summary_data$date, date_format) + 1/12)
weather_summary_data$Season <- factor(format(yq, "%q"), levels=1:4,
                                      labels=c('winter','spring','summer','fall'))

############ Annual weather summaries ##############
# For each variable, aggregate by water year (mean for tmin and tmax, sum for precip and snow)
# By default, aggregate function creates non-intuitive headers, so we manually rename the columns 
# basic plots are provided

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

# Compile into annual data frame to be exported as CSV
weather_annual_compiled <- data.frame(WaterYear=WY_precip$WaterYear, Precip_mm=WY_precip$daily_precip_mm,
                                      Tmax_C=WY_tmax$max_dailytemp_C, Tmin_C=WY_tmin$min_dailytemp_C,
                                      Snow_mm=WY_snowfall$daily_snowfall_mm)
# NOTE: removing 2017 because only had fall 2016 data, but keeping 2005 for now because data begin in May 2005
weather_annual_compiled <- subset(weather_annual_compiled, WaterYear <= 2016)
#write.csv(weather_annual_compiled, file=paste0(getwd(),"/Datasets/Sunapee/SummarizedData/ExSitu/Newport_weather_annual.csv"))

#################### monthly weather summaries ##########################
# create new column based on just Month-year combinations
weather_summary_data$MonthYear <- format(as.Date(weather_summary_data$date), "%Y-%m")

# aggregate precip and snow by month, then rename columns
monthly_precip_snow <- aggregate(cbind(weather_summary_data$daily_precip_mm, weather_summary_data$daily_snowfall_mm),
                                  by=list(weather_summary_data$MonthYear),FUN=sum, na.rm=T)
colnames(monthly_precip_snow) <- c('MonthYear','Precip_mm','Snow_mm')

# aggregate tmin and tmax by month, then rename columns
monthly_temp <- aggregate(cbind(weather_summary_data$max_dailytemp_C, weather_summary_data$min_dailytemp_C),
                                 by=list(weather_summary_data$MonthYear),FUN=mean, na.rm=T)

colnames(monthly_temp) <- c('MonthYear','Tmax_C','Tmin_C')

# compile monthly weather data into new data frame to be exported as CSV
weather_monthly_compiled <- data.frame(MonthYear=monthly_precip_snow$MonthYear,Precip_mm=monthly_precip_snow$Precip_mm,
                                       Tmax_C=monthly_temp$Tmax_C, Tmin_C=monthly_temp$Tmin_C, Snow_mm=monthly_precip_snow$Snow_mm)

#write.csv(weather_monthly_compiled, file=paste0(getwd(),"/Datasets/Sunapee/SummarizedData/ExSitu/Newport_weather_monthly.csv"))

#################### weekly weather summaries #########################

# create week column and YearWeek column to distinguish weeks across years
weather_summary_data$Week <- week(weather_summary_data$date)
weather_summary_data$YearWeek <- paste0(weather_summary_data$CalYear,"_Week",weather_summary_data$Week)

# aggregate precip and snow by week, then rename columns
weekly_precip_snow <- aggregate(cbind(weather_summary_data$daily_precip_mm, weather_summary_data$daily_snowfall_mm),
                                         by=list(weather_summary_data$YearWeek),FUN=sum, na.rm=T)
colnames(weekly_precip_snow) <- c('YearWeek','Precip_mm','Snow_mm')

# aggregate tmax and tmin by week, then rename columns
weekly_temp <- aggregate(cbind(weather_summary_data$max_dailytemp_C, weather_summary_data$min_dailytemp_C),
                          by=list(weather_summary_data$YearWeek),FUN=median, na.rm=T)
colnames(weekly_temp) <- c('YearWeek','Tmax_C','Tmin_C')

# compile weekly data into new data frame to be exported as CSV
weather_weekly_compiled <- data.frame(YearWeek=weekly_precip_snow$YearWeek, Precip_mm=weekly_precip_snow$Precip_mm,
                                      Tmax_C=weekly_temp$Tmax_C, Tmin_C=weekly_temp$Tmin_C,
                                      Snow_mm=weekly_precip_snow$Snow_mm)
#write.csv(weather_weekly_compiled, file=paste0(getwd(),"/Datasets/Sunapee/SummarizedData/ExSitu/Newport_weather_weekly.csv"))

################ Seasonal weather summaries ##################
# from early 2018 exploratory data analyses
# precip
seasonal_precip <- aggregate(weather_summary_data$daily_precip_mm, by=list(weather_summary_data$WaterYear,weather_summary_data$Season), FUN=sum, na.rm=T)
colnames(seasonal_precip) <- c('WaterYear','Season','Precip_mm')

ggplot(seasonal_precip, aes(factor(WaterYear), Precip_mm, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Newport Station', x='water Year',y='Precipitation (mm)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))

# tmin
seasonal_tmin <- aggregate(weather_summary_data$min_dailytemp_C, by=list(weather_summary_data$WaterYear,weather_summary_data$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmin) <- c('WaterYear','Season','tmin_C')

ggplot(seasonal_tmin, aes(factor(WaterYear), tmin_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Newport Station', x='water Year',y='tmin (C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))

# tmax
seasonal_tmax <- aggregate(weather_summary_data$max_dailytemp_C, by=list(weather_summary_data$WaterYear,weather_summary_data$Season), FUN=mean, na.rm=T)
colnames(seasonal_tmax) <- c('WaterYear','Season','tmax_C')

ggplot(seasonal_tmax, aes(factor(WaterYear), tmax_C, fill = Season)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(title='Newport Station', x='water Year',y='tmax (C)') +
  scale_fill_manual('legend',values=c('winter'='steelblue','spring'='olivedrab','summer'='gold','fall'='sienna'))

#command to save plots
#ggsave("Newport_weather.pdf",width=11, height=8.5)
