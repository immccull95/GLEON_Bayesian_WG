############################ Lake Sunapee precip vs. gloeo across sites #############################
# Date: 6-4-18
# updated:
# Authors: Ian McCullough 
#####################################################################################################

#### R libraries ####
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)

#### set working directory ####
#please comment out and make new line for your own directory
setwd("C:/Users/FWL/Documents/GLEON_Bayesian_WG") #Ian's working directory

#### input data ####
weather_monthly <- read.csv(paste0(getwd(), "/Datasets/Sunapee/SummarizedData/ExSitu/Newport_weather_monthly.csv"))
weather_weekly <- read.csv(paste0(getwd(), "/Datasets/Sunapee/SummarizedData/ExSitu/Newport_weather_weekly.csv"))
weather_annual <- read.csv(paste0(getwd(), "/Datasets/Sunapee/SummarizedData/ExSitu/Newport_weather_annual.csv"))

coffin_gloeo <- read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='coffin_weeklygloeo')
fichter_gloeo <- read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='fichter_weeklygloeo')
midge_gloeo <- read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='midge_weeklygloeo')
newbury_gloeo <- read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='newbury_weeklygloeo')


#### define functions ####
# this function creates a basic plot of annual total precip vs. annual median gloeo
annual_precip_gloeo <- function(weather_df, gloeo_df, sitename){
  #precip_df: contains precip column and WaterYear
  #gloeo_df: contains gloeo column (coloniesperL) and Year
  #sitename: text in quotes
  annual_precip <- aggregate(weather_df$Precip_mm, by=list(weather_df$WaterYear), FUN=sum, na.rm=T)
  colnames(annual_precip) <- c('Year','Precip_mm')
  site_gloeo_annual <- aggregate(gloeo_df$coloniesperL, by=list(gloeo_df$year), FUN=median, na.rm=T)
  colnames(site_gloeo_annual) <- c('Year','coloniesperL')
  site_gloeo_precip_annual <- merge(annual_precip, site_gloeo_annual, by='Year', all.x=F)
  plot(Precip_mm ~ coloniesperL, data=site_gloeo_precip_annual, pch=20, main=sitename,
       xlab='Median coloniesperL', ylab='Precip (mm)')
  rval <- round(cor(site_gloeo_precip_annual$Precip_mm, site_gloeo_precip_annual$coloniesperL),2)
  linmodel <- lm(Precip_mm ~ coloniesperL, data=site_gloeo_precip_annual)
  pval <- round(summary(linmodel)$coefficients[,4][2],2)
  legend('topright', legend=paste0("r = ",rval, ", p = ", pval), bty='n')
  mtext(side=3, paste0("nYears = ",nrow(site_gloeo_precip_annual)))
  abline(linmodel)
}

# this somewhat more complex function plots monthly total precip vs. monthly median gloeo
# any potential "year effects" are not accounted for; e.g., all Junes were lumped together, etc.
monthly_precip_gloeo <- function(weather_df, gloeo_df, sitename){
  #precip_df: contains precip column and MonthYear
  #gloeo_df: contains gloeo column (coloniesperL) and Year
  #sitename: text in quotes
  gloeo_df$MonthYear <- zoo::as.Date(zoo::as.yearmon(gloeo_df$date, "%y-%b"))
  monthly_precip <- aggregate(weather_df$Precip_mm, by=list(weather_df$MonthYear), FUN=sum, na.rm=T)
  colnames(monthly_precip) <- c('MonthYear','Precip_mm')
  site_gloeo_monthly <- aggregate(gloeo_df$coloniesperL, by=list(gloeo_df$MonthYear), FUN=median, na.rm=T)
  colnames(site_gloeo_monthly) <- c('MonthYear','coloniesperL')
  site_gloeo_precip_monthly <- merge(monthly_precip, site_gloeo_monthly, by='MonthYear', all.x=F)
  site_gloeo_precip_monthly$Month <- month(site_gloeo_precip_monthly$MonthYear, label=T) #create month column for plotting points by month
  site_gloeo_precip_monthly[] <- lapply(site_gloeo_precip_monthly, function(x) if(is.factor(x)) factor(x) else x) #remove unused factor levels
  rval <- round(cor(site_gloeo_precip_monthly$Precip_mm, site_gloeo_precip_monthly$coloniesperL),2)
  linmodel <- lm(Precip_mm ~ coloniesperL, data=site_gloeo_precip_monthly)
  pval <- round(summary(linmodel)$coefficients[,4][2],2)
  
  ggplot(site_gloeo_precip_monthly)+
    geom_point(aes(x=coloniesperL,y=Precip_mm,colour=Month))+
    ggtitle(paste0(sitename, ', r = ',rval, ', p = ',pval, ', nMonths = ', nrow(site_gloeo_precip_monthly))) +
    theme_bw()
}

################################# Main program #########################################
# Exploratory analysis
# compare precip at different time scales to gloeo at different sites

## annual gloeo vs. annual precip
annual_precip_gloeo(weather_df=weather_annual, gloeo_df=coffin_gloeo, sitename='Coffin')
annual_precip_gloeo(weather_df=weather_annual, gloeo_df=fichter_gloeo, sitename='Fichter')
annual_precip_gloeo(weather_df=weather_annual, gloeo_df=midge_gloeo, sitename='Midge')
annual_precip_gloeo(weather_df=weather_annual, gloeo_df=newbury_gloeo, sitename='Newbury')

# monthly gloeo vs. monthly precip
weather_monthly$Date <- ymd(weather_monthly$MonthYear, truncated = 1L)
weather_monthly$Month <- month(weather_monthly$Date)
weather_monthly$MonthYear <- zoo::as.Date(zoo::as.yearmon(weather_monthly$Date, "%y-%b"))

monthly_precip_gloeo(weather_df=weather_monthly, gloeo_df=coffin_gloeo, sitename='Coffin')
monthly_precip_gloeo(weather_df=weather_monthly, gloeo_df=fichter_gloeo, sitename='Fichter')
monthly_precip_gloeo(weather_df=weather_monthly, gloeo_df=midge_gloeo, sitename='Midge')
monthly_precip_gloeo(weather_df=weather_monthly, gloeo_df=newbury_gloeo, sitename='Newbury')
