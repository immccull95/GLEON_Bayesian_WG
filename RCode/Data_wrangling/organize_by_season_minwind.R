#Data Wrangling for Model Runs
#Author: Mary Lofton
#Date: 12APR19

#Purpose: align all years and sites to have the same number of sampling weeks starting and ending
#at the same time of year so we can run a logistic model in JAGS year-by-year

#then make a k by j matrix for driver data and environmental inputs

library(tidyverse)
library(lubridate)

dat <- read_csv("./Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp_airtemp.csv")

season <- ggplot(data = dat, aes(x = week, y = year, colour = as.factor(year), group = year))+
  geom_line(size = 1.5)+
  facet_wrap(~site, ncol = 2, nrow = 2)+
  theme_bw()
season

cleaned_dat <- dat %>%
  mutate(week = week(date),
         dayofyr = yday(date)) %>%
  filter(year %in% 2009:2014 & (week %in% 21:40 | dayofyr == 283))

bad_dates <- as.Date(c("2009-05-21","2009-05-28","2009-06-18",
                       "2009-09-03","2009-09-10","2009-09-17",
                       "2009-10-01","2010-05-27","2010-06-03","2010-06-17","2011-06-09",
                       "2012-10-04","2013-07-04","2013-09-12","2013-09-19","2013-09-26",
                       "2013-10-03","2014-05-22","2014-07-03","2014-07-10","2014-07-24",
                       "2014-07-31","2014-08-07","2014-08-14","2014-08-21", "2014-08-28",
                       "2014-09-11","2014-09-18","2014-10-02","2013-10-10"))

cleaned_dat1 <- cleaned_dat[-c(193:194),] %>%
  filter(!date %in% bad_dates) %>%
  arrange(year,date,site) %>%
  mutate(season_week = rep(c(1:20),times = 6, each = 4))

write.csv(cleaned_dat1, file = "./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv", row.names = FALSE)

years <- c(2009:2014)

for (i in 1:length(years)){
  check <- cleaned_dat1 %>% filter(year == years[2])
  print(length(unique(check$date)))
}

#ok. 2009 is missing values for wk of 6-10 and wk of 9-28
#2010 has one week where 2 sites were samples 9-22 and 
#2 sites were samples 9-23 (so still 20 wks)
#2014 is missing one observation at newbury wk of 7-17

#now the tedious process of cleaning week-by-week


         
#check <- cleaned_dat1 %>% filter(is.na(TOBS))
#length == 0 == HOORAY!

season_check <- ggplot(data = cleaned_dat1, aes(x = season_week, y = year, colour = as.factor(year), group = year))+
  geom_line(size = 1.5)+
  facet_wrap(~site, ncol = 2, nrow = 2)+
  theme_bw()
season_check



#get in shape for seasonal for-loop
gloeo_seasonal <- cleaned_dat1 %>%
  filter(site == "midge") %>%
  select(year, season_week, totalperL) %>%
  #mutate(totalperL = round(totalperL*141.3707)) %>%
  spread(key = season_week, value = totalperL) %>%
  select(-year)

colnames(gloeo_seasonal) <- paste("wk", colnames(gloeo_seasonal), sep = "_")

write.csv(gloeo_seasonal, "./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_totalperL_31JUL19.csv", row.names = FALSE)

###########now for wind
cleaned_dat2 <- cleaned_dat1 %>%
  filter(site == "midge")
  
wind <- read_csv("./Datasets/Sunapee/SummarizedData/gloeo_Midge_instwindsp_filtered.csv") %>%
  select(date, WindSp_ms_min, WindSp_ms_cv)

wind1 <- left_join(cleaned_dat2,wind, by = "date")

#get in shape for seasonal for-loop - MINWIND
minwind_seasonal <- wind1 %>%
  select(year, season_week, WindSp_ms_min) %>%
  spread(key = season_week, value = WindSp_ms_min) %>%
  select(-year)

colnames(minwind_seasonal) <- paste("wk", colnames(minwind_seasonal), sep = "_")

write.csv(minwind_seasonal, "./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_minwind_24FEB20.csv", row.names = FALSE)

#get in shape for seasonal for-loop - CVWIND
CVwind_seasonal <- wind1 %>%
  select(year, season_week, WindSp_ms_cv) %>%
  spread(key = season_week, value = WindSp_ms_cv) %>%
  select(-year)

colnames(CVwind_seasonal) <- paste("wk", colnames(CVwind_seasonal), sep = "_")

write.csv(CVwind_seasonal, "./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_CVwind_24FEB20.csv", row.names = FALSE)

############for forecasting
#######Gloeo data for uncertainty partitioning
dat <- read_csv("./Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp_airtemp.csv")

season <- ggplot(data = dat, aes(x = week, y = year, colour = as.factor(year), group = year))+
  geom_line(size = 1.5)+
  facet_wrap(~site, ncol = 2, nrow = 2)+
  theme_bw()
season

cleaned_dat <- dat %>%
  mutate(week = week(date),
         dayofyr = yday(date)) %>%
  filter(year %in% 2009:2016 & (week %in% 21:40 | dayofyr == 283))

bad_dates <- as.Date(c("2009-05-21","2009-05-28","2009-06-18",
                       "2009-09-03","2009-09-10","2009-09-17",
                       "2009-10-01","2010-05-27","2010-06-03","2010-06-17","2011-06-09",
                       "2012-10-04","2013-07-04","2013-09-12","2013-09-19","2013-09-26",
                       "2013-10-03","2014-05-22","2014-07-03","2014-07-10","2014-07-24",
                       "2014-07-31","2014-08-07","2014-08-14","2014-08-21", "2014-08-28",
                       "2014-09-11","2014-09-18","2014-10-02","2013-10-10", "2015-06-04",
                       "2015-06-18","2015-08-27","2015-10-01","2015-10-10","2016-06-02",
                       "2016-07-28","2016-08-18","2016-08-25","2016-09-01","2016-09-08",
                       "2016-09-15","2016-09-22","2016-09-29"))

cleaned_dat1 <- cleaned_dat[-c(193:194),] %>%
  filter(!date %in% bad_dates) %>%
  arrange(year,date,site) %>%
  mutate(season_week = rep(c(1:20),times = 8, each = 4))

#get in shape for seasonal for-loop - MINWIND
cleaned_dat2 <- cleaned_dat1 %>%
  filter(site == "midge")

wind <- read_csv("./Datasets/Sunapee/SummarizedData/gloeo_Midge_instwindsp_filtered.csv") %>%
  select(date, WindSp_ms_min)

wind1 <- left_join(cleaned_dat2,wind, by = "date")

minwind_seasonal_0 <- wind1 %>%
  select(year, date, season_week, WindSp_ms_min)

write.csv(minwind_seasonal_0, "./Datasets/Sunapee/SummarizedData/Midge_year_by_week_minwind_forecast_25FEB20.csv", row.names = FALSE)

minwind_seasonal <- wind1 %>%
  select(year, season_week, WindSp_ms_min) %>%
  spread(key = season_week, value = WindSp_ms_min) %>%
  select(-year)

colnames(minwind_seasonal) <- paste("wk", colnames(minwind_seasonal), sep = "_")

write.csv(minwind_seasonal, "./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_minwind_24FEB20.csv", row.names = FALSE)

