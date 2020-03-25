#Data Wrangling for Model Runs
#Author: Mary Lofton
#Date: 12APR19

#Purpose: align all years and sites to have the same number of sampling weeks starting and ending
#at the same time of year so we can run a logistic model in JAGS year-by-year

#then make a k by j matrix for driver data and environmental inputs
#(didn't include this step in this version of the script Jennie since I think you work in long format)

library(tidyverse)
library(lubridate)

dat <- read_csv("./Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp_airtemp.csv")

#check to see how many weeks of data each season has - you can see they are not all the same
season <- ggplot(data = dat, aes(x = week, y = year, colour = as.factor(year), group = year))+
  geom_line(size = 1.5)+
  facet_wrap(~site, ncol = 2, nrow = 2)+
  theme_bw()
season

cleaned_dat <- dat %>%
  mutate(week = week(date),
         dayofyr = yday(date)) %>%
  filter(year %in% 2009:2014 & (week %in% 21:40 | dayofyr == 283))

#after doing a bunch of manual checks, these are dates that were somehow created 
#during one of our interpolation steps but have no associated GLOEO data
#so they need to be removed
bad_dates <- as.Date(c("2009-05-21","2009-05-28","2009-06-18",
                       "2009-09-03","2009-09-10","2009-09-17",
                       "2009-10-01","2010-05-27","2010-06-03","2010-06-17","2011-06-09",
                       "2012-10-04","2013-07-04","2013-09-12","2013-09-19","2013-09-26",
                       "2013-10-03","2014-05-22","2014-07-03","2014-07-10","2014-07-24",
                       "2014-07-31","2014-08-07","2014-08-14","2014-08-21", "2014-08-28",
                       "2014-09-11","2014-09-18","2014-10-02","2013-10-10"))

#manually removing crummy dates
cleaned_dat1 <- cleaned_dat[-c(193:194),] %>%
  filter(!date %in% bad_dates) %>%
  arrange(year,date,site) %>%
  mutate(season_week = rep(c(1:20),times = 6, each = 4))

#for-loop to make sure we only have 20 dates for every sample year
#actually you will see 21 dates for each year because there is one sampling
#day where some of the sites were sampled the next day, so that is fine
years <- c(2009:2014)

for (i in 1:length(years)){
  check <- cleaned_dat1 %>% filter(year == years[1])
  print(length(unique(check$date)))
}

#double-double-checking that all the years have the same length sampling period
season_check <- ggplot(data = cleaned_dat1, aes(x = season_week, y = year, colour = as.factor(year), group = year))+
  geom_line(size = 1.5)+
  facet_wrap(~site, ncol = 2, nrow = 2)+
  theme_bw()
season_check

#now just select what we really need: logged Gloeo at Midge
cleaned_dat2 <- cleaned_dat1 %>%
  filter(site == "midge")%>%
  select(date, totalperL)%>%
  mutate(totalperL_log = log(totalperL + 0.003))
  
write.csv(cleaned_dat2, file = "./Jennies/awesome/filepath/data.csv", row.names = FALSE)


