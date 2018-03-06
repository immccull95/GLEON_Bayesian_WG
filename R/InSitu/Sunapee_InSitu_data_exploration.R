############################ Lake Sunapee IN situ data exploration #############################
# Date: 1-1-18
# updated:2-28-18
# Authors: JAB, MEL
################################################################################################
### Gloeo exploratory analysis
#Created 1 January 2018 - JAB

#### Set Working Directory ####
setwd("~/Documents/GLEON_Bayesian_WG/Datasets/Sunapee/R Work")

#### Install R Packages ####
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(lubridate)
#In Tidy Verse
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)


#### Read in data for all sites from Shannon weekly summary ####
#JAB updated Shannon weekly summary to include only observation per week
#Sheet tells R what to pull from on the excel document which is handy insted of
#loading multiple csv's
coffin_gloeo = read_excel("Sunapee_weeklysummary_JBedits.xlsx", sheet='coffin_weeklygloeo')
fichter_gloeo = read_excel("Sunapee_weeklysummary_JBedits.xlsx", sheet='fichter_weeklygloeo')
midge_gloeo = read_excel("Sunapee_weeklysummary_JBedits.xlsx", sheet='midge_weeklygloeo')
newbury_gloeo = read_excel("Sunapee_weeklysummary_JBedits.xlsx", sheet='newbury_weeklygloeo')

#check the data structure
str(midge_gloeo)

#Merge datasets for all sites into one file
all_sites_gloeo = bind_rows(coffin_gloeo,fichter_gloeo,midge_gloeo,newbury_gloeo)
write_csv(all_sites_gloeo, "All_Sites_Gloeo.csv")
summary(all_sites_gloeo)

#Merge in-situ data from newbury and midge with gloeo data
midge_insitu = read_excel("Sunapee_weeklysummary.xlsx", sheet='midge_insitu_data')
newbury_insitu = read_excel("Sunapee_weeklysummary.xlsx", sheet='newbury_insitu_data')

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

#Read in water temp data combined and numeric
watertemp = read.csv("Sunapee_watertemp.csv")
#Conver water temp to long data
watertemp_long <- watertemp %>%
  select(week:newbury.median) %>%
  filter(!is.na(coffin.mean)) %>%
  gather(key=site, value = watertemp_c, coffin.mean:newbury.median) %>%
  separate(col=site,into = c("site","method")) %>%
  spread(key=method,value=watertemp_c) %>%
  arrange(year,site)

watertemp_midge <- watertemp %>%
  select(week,year,midge.mean:midge.median) %>%
  filter(!is.na(midge.mean)) %>%
  gather(key=site, value = watertemp_c, midge.mean:midge.median) %>%
  separate(col=site,into = c("site","method")) %>%
  spread(key=method,value=watertemp_c)

watertemp_all_long <- bind_rows(watertemp_long,watertemp_midge) %>%
  arrange(year,site)

write_csv(watertemp_all_long,"watertemp_all_long.csv")  

#Merge midge and newbury all with water temp, air temp, precip

midge_all = read_csv("midge_all.csv")
newbury_all = read_csv("newbury_all.csv")

midge_all2 <- left_join(midge_all,watertemp_midge,by = c("year", "week"))
  


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


#### Boxplot ####

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


