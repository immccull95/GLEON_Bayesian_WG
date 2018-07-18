############################ Lake Sunapee IN situ data exploration #############################
# Date: 1-1-18
# updated:06-26-18 by LSB
# Authors: JAB, MEL
################################################################################################
### Gloeo exploratory analysis
#Created 1 January 2018 - JAB

#### Install R Packages ####
library(tidyverse)
library(readxl)
library(lubridate)
library(doBy) #included by LSB to aggregate water temp data following Bethel Steele code.

### Set working directory to be the "GLEON_Bayesian_WG" folder
setwd("~/GitHub/GLEON_Bayesian_WG") #may need to change according to the user's folder path

#### Read in data for all sites from Shannon weekly summary ####
#JAB updated Shannon weekly summary to include only 1 observation per week
#Sheet tells R what to pull from on the excel document which is handy insted of
#loading multiple csv's
coffin_gloeo = read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='coffin_weeklygloeo')
fichter_gloeo = read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='fichter_weeklygloeo')
midge_gloeo = read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='midge_weeklygloeo')
newbury_gloeo = read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='newbury_weeklygloeo')

#check the data structure
str(midge_gloeo)

#Merge datasets for all sites into one file
all_sites_gloeo = bind_rows(coffin_gloeo,fichter_gloeo,midge_gloeo,newbury_gloeo)
write_csv(all_sites_gloeo, "All_Sites_Gloeo.csv")
summary(all_sites_gloeo)

#Merge in-situ data from newbury and midge with gloeo data
midge_insitu = read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='midge_insitu_data')
newbury_insitu = read_excel("Datasets/Sunapee/R Work/Level 1/Sunapee_weeklysummary_JBedits.xlsx", sheet='newbury_insitu_data')

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



############################# WATER TEMP AGGREGATION #################################
#Updated by LSB 26-June-2018

#### Read in water temp data ####
watertemp_hourly = read_csv("Datasets/Sunapee/R Work/Level 1/temp_2006-2016_L1_20Oct2017.csv", col_types = cols(
  coffin = col_double(),
  fichter = col_double(),
  newbury = col_double()))
str(watertemp_hourly)

# 2009 data - readings every 30 min so filtered out to only include hourly readings
watertemp_hourly_true <- watertemp_hourly %>% 
  mutate(minute = minute(datetime)) %>% 
  filter(minute == 0)

#### calculate weekly, monthly and  annual summaries ####
#Add in week, month and year
temp_L1 <- watertemp_hourly_true[,1:9] %>%
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date)) #this way we are considering week numbers as the the number of complete seven day periods that have occurred between the date and January 1st, plus one.

#Aggregate by week number, month, year and sampling period
sumfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), min=min(x, na.rm=TRUE, ...), max=max(x, na.rm=TRUE, ...), 
    median=median(x, na.rm=TRUE, ...), obs=sum(!is.na(x)))}
temp_L1 <- as.data.frame(temp_L1)

watertemp_week <- summaryBy(coffin + fichter + newbury + midge ~ year + week, data=temp_L1, FUN=sumfun)
watertemp_month <- summaryBy(coffin + fichter + newbury + midge ~ year + month, data=temp_L1, FUN=sumfun)

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
write_csv(watertemp_week,"Datasets/Sunapee/R Work/Level 1/watertemp_week.csv")
write_csv(watertemp_month,"Datasets/Sunapee/R Work/Level 1/watertemp_month.csv")
write_csv(watertemp_year,"Datasets/Sunapee/R Work/Level 1/watertemp_year.csv")
write_csv(watertemp_sampling,"Datasets/Sunapee/R Work/Level 1/watertemp_sampling.csv")

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


#### Read in final Midge weekly data and water temp to combine ####
#Combine Midge weekly data with water temp
midge_weekly = read_csv("Datasets/Sunapee/R Work/Level 1/midge_in-situ_weekly.csv")
watertemp = read_csv("Datasets/Sunapee/R Work/Level 1/Sunapee_watertemp.csv") #update with new weekly water temp data

watertemp_midge <- watertemp %>%
  select(week,year,midge.mean:midge.median) %>%
  filter(!is.na(midge.mean)) %>%
  gather(key=site, value = watertemp_c, midge.mean:midge.median) %>%
  separate(col=site,into = c("site","method")) %>%
  spread(key=method,value=watertemp_c) %>% 
  arrange(year)

midge_all_temp <- left_join(midge_weekly,watertemp_midge,by = c("year", "week")) %>%
  select(-site.y)

write_csv(midge_all_temp, "Datasets/Sunapee/R Work/Level 1/midge_all_temp.csv")



#### Read in light dataset ####

light_allsites <- read_csv("Datasets/Sunapee/R Work/Level 1/templight_0916_L1_4sites_30Oct2017-JBedits.csv", col_types = cols(
  temp_Coffin = col_double(),
  temp_Fichter = col_double(),
  temp_OldNewbury = col_double(),
  light_Coffin = col_double(),
  light_Fichter = col_double(),
  light_OldNewbury = col_double()))

str(light_allsites)

#Count number of observations for each column

light_allsites_count <- light_allsites %>% 
  mutate(week = week(datetime)) %>%
  select(year,temp_Coffin:week) %>% 
  group_by(year,week) %>% 
  summarize(count = n())

light_allsites <- light_allsites %>% 
  mutate(week = week(datetime)) %>%
  mutate(week_day = wday(datetime)) %>% 
  mutate(day = day(datetime))


#Separate out 10 min readings at 0 min vs. 7 min 2 min 9 min 8 1 min 3 4min
light_10min <- light_allsites %>%
  mutate(minute = minute(time)) %>%
  filter(minute %in% c(0,10,20,30,40,50))

write_csv(light_10min,"Datasets/Sunapee/R Work/Level 1/light_10min.csv")
light_10min <- read_csv("Datasets/Sunapee/R Work/Level 1/light_10min.csv", col_types = cols(
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

write_csv(light_newtime5,"Datasets/Sunapee/R Work/Level 1/light_newtime_joinall.csv")

#Calculate weekly median, mean, max for light & HOBO temp data at all sites

light_summary_10min <- light_10min %>% 
  mutate(month = month(datetime)) %>% 
  mutate(week = week(datetime)) %>% 
  group_by(year,week) %>% 
  summarize_at(vars(temp_Coffin:light_OldNewbury),funs(mean, median, max (.,na.rm=T)))
  
write_csv(light_summary_10min,"Datasets/Sunapee/R Work/Level 1/light_temp_weekly_summary_10min_take2.csv")



# Read in summarize dataset with -Inf changed to NA
light_summary <- read_csv("Datasets/Sunapee/R Work/Level 1/light_temp_weekly_summary.csv")

light_temp_long <- light_summary %>%
  gather(key=site, value = light, temp_Coffin_mean:light_OldNewbury_max) %>%
  separate(col=site,into = c("measure","site","method")) %>%
  spread(key=method,value=light) %>%
  arrange(year,site)

write_csv(light_temp_long,"Datasets/Sunapee/R Work/Level 1/light_temp_weekly_summary-long.csv")

#Filter for just Midge light data for now
midge_light <- light_temp_long %>% 
  filter(site=="Midge",measure=="light")


# #### Monthly Summary ####
#Comented by LSB 14-May-2018, monthly and year aggregation foloow steps on Water Temp section code above
# # Calculate monthly 
# midge_weekly = read_csv("Datasets/Sunapee/R Work/Level 1/midge_in-situ_weekly.csv")
# 
# 
# # Read in hourly temp data and calculate monthly summary
# watertemp_hourly = read_csv("Datasets/Sunapee/R Work/Level 1/temp_2006-2016_L1_20Oct2017.csv", col_types = cols(
#   coffin = col_double(),
#   fichter = col_double(),
#   newbury = col_double()))
# 
# str(watertemp_hourly)
# 
# # 2009 data - readings every 30 min so filtered out to only include hourly readings
# watertemp_hourly_true <- watertemp_hourly %>% 
#   mutate(minute = minute(datetime)) %>% 
#   filter(minute == 0)
# 
# #count to check number of values per month before averaging - 30 days = 720 readings, 31 days = 744
# watertemp_count_coffin <- watertemp_hourly_true %>% 
#   mutate(month = month(datetime)) %>% 
#   select(year,month,coffin) %>% 
#   group_by(year,month) %>% 
#   summarize(site_count = n())
# 
# watertemp_count_fichter <- watertemp_hourly_true %>% 
#   mutate(month = month(datetime)) %>% 
#   filter(!is.na(fichter)) %>% 
#   select(year,month,fichter) %>% 
#   group_by(year,month) %>% 
#   summarize(site_count = n())
# 
# # Monthly summary
# watertemp_monthly <- watertemp_hourly_true %>% 
#   mutate(month = month(datetime)) %>% 
#   group_by(year,month) %>% 
#   summarize_at(vars(coffin:midge),funs(mean, median, min, max (.,na.rm=T)))
# 
# #Replace -Inf values with NA for all data
# watertemp_monthly2 <- replace(watertemp_monthly, x == -Inf, NA)

  
############### FIGURES ###############

#### Figures for gloeo all sites - facet wrap by year ####
#I plotted day of year on the x axis and total colonies per L on the y
#colors are for the different sites and each panel represents a different year
#the rest of the code below that is mostly formatting for a pretty figure
ggplot(all_sites_gloeo, aes(x=dayofyr,y=totalperL,color=site))+
  geom_line(size=1.5)+
  scale_colour_manual(values=c("black","forestgreen","blue","orange"))+ 
  #scale_x_datetime(breaks = seq(as.POSIXct("2005-08-01"),as.POSIXct("2016-10-01"), by="1 year"),date_labels="%Y")+ #%d-%b-%y
  scale_x_continuous(limits=c(47,300), breaks=seq(50,300, by=50))+ #%d-%b-%y
  labs(y="Gloeotrichia Surface Abundance (colonies/L)", x="Day of Year")+
  facet_wrap(~year,scales ="free_y")+
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

#Saves figure as a pdf with set dimensions in the working directory folder
ggsave("All_Sites_gloeo-byyear_2005-2016.pdf",width=15, height=8.5)

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

ggsave("All_Sites_gloeo_2005-2016.pdf",width=15, height=8.5)


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

ggsave("All_Sites_minwatertemp-byyear_2007-2016.pdf",width=15, height=8.5)


#### Correlation Analysis ####

#cor() pearson used by default but can call spearman or kendall