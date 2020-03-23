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

###########now for minwind
cleaned_dat2 <- cleaned_dat1 %>%
  filter(site == "midge")
  
wnd <- read_csv("./Datasets/Sunapee/RawData/NLDAS_SunapeeMet_1979_2016.csv") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(wind_min_daily = min(WindSpeed_mpersec, na.rm = TRUE))

wnd1 <- left_join(cleaned_dat2, wnd, by = "date")

plot(wnd1$wind_min_daily,log(wnd1$totalperL))

ggplot(data = wnd1, aes(x = season_week, y = wind_min_daily))+
  geom_point(size = 1)+
  geom_line(size = 1)+
  facet_wrap(~year, ncol = 3, nrow = 2)+
  theme_classic()

#get in shape for seasonal for-loop - CVWIND
wnd_seasonal <- wnd1 %>%
  select(year, season_week, wind_min_daily) %>%
  spread(key = season_week, value = wind_min_daily) %>%
  select(-year)

colnames(wnd_seasonal) <- paste("wk", colnames(wnd_seasonal), sep = "_")

write.csv(wnd_seasonal, "./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_NLDASminwind_03MAR20.csv", row.names = FALSE)

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

###########now for minwind
cleaned_dat2 <- cleaned_dat1 %>%
  filter(site == "midge")

wnd <- read_csv("./Datasets/Sunapee/RawData/NLDAS_SunapeeMet_1979_2016.csv") %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarize(wind_min_daily = min(WindSpeed_mpersec, na.rm = TRUE))

wnd1 <- left_join(cleaned_dat2, wnd, by = "date")

plot(wnd1$wind_min_daily,log(wnd1$totalperL))

ggplot(data = wnd1, aes(x = season_week, y = wind_min_daily))+
  geom_point(size = 1)+
  geom_line(size = 1)+
  facet_wrap(~year, ncol = 4, nrow = 2)+
  theme_classic()

#get in shape for seasonal for-loop - CVWIND
wnd_seasonal <- wnd1 %>%
  select(year, season_week, wind_min_daily) %>%
  spread(key = season_week, value = wind_min_daily) %>%
  select(-year)

colnames(wnd_seasonal) <- paste("wk", colnames(wnd_seasonal), sep = "_")

write.csv(wnd_seasonal, "./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_NLDASminwind_forecast_03MAR20.csv", row.names = FALSE)
