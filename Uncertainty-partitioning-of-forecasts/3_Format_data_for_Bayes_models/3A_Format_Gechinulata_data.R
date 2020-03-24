#Title: 3A Format G. echinulata data for Bayesian models
#Author: Mary Lofton
#Date: 23MAR20

#Load packages

#run this line if you do not have pacman installed
#install.packages('pacman')

#load other packages
pacman::p_load(tidyverse, lubridate)

#load data file
dat <- read_csv("./Uncertainty-partitioning-of-forecasts/00_Data_files/All_Sites_Gloeo_20Mar2020.csv") 

#insert rows for missing observations in 2009-2016
date <- c(rep(c("2009-06-11","2009-09-28","2015-06-25","2016-06-09","2016-08-10"),times = 1,each = 4),"2014-07-17","2015-06-11","2015-07-23","2015-08-20")
site <- c(rep(c("North_Sunapee_Harbor","South_of_the_Fells","South_Herrick_Cove","Newbury"),times = 5),"Newbury","South_of_the_Fells","South_Herrick_Cove","South_of_the_Fells")

missing_obs <- data.frame(cbind(date,site))
missing_obs$coloniesperL <- NA
missing_obs$filbundperL <- NA
missing_obs$totalperL <- NA

#combine missing observations with original data file
#select dates in 2009-2016
#assign "sampling season weeks" numbering 1-20 each year to dates
dat1 <- rbind(dat,missing_obs) %>%
  mutate(week = week(date),year = year(date), dayofyr = yday(date)) %>%
  filter(year %in% 2009:2016 & (week %in% 21:40 | dayofyr == 283))%>%
  arrange(date, site) %>%
  select(date, site, coloniesperL, filbundperL, totalperL, year) %>%
  mutate(season_week = rep(c(1:20),times = 8, each = 4))

#get in wide format (year by week) for seasonal for-loop in JAGS models

#Site 1 (focal site for analysis)
dat2 <- dat1 %>%
  filter(site == "South_Herrick_Cove") %>%
  select(year, season_week, totalperL) %>%
  spread(key = season_week, value = totalperL) %>%
  select(-year)

colnames(dat2) <- paste("wk", colnames(dat2), sep = "_")

write.csv(dat2, "./Uncertainty-partitioning-of-forecasts/00_Data_files/Gechinulata_Site1.csv", row.names = FALSE)

