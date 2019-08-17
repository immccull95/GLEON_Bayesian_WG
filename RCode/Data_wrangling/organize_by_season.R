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
  filter(site == "fichter") %>%
  select(year, season_week, totalperL) %>%
  #mutate(totalperL = round(totalperL*141.3707)) %>%
  spread(key = season_week, value = totalperL) %>%
  select(-year)

colnames(gloeo_seasonal) <- paste("wk", colnames(gloeo_seasonal), sep = "_")

write.csv(gloeo_seasonal, "./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_totalperL_31JUL19.csv", row.names = FALSE)


#same thing for air temp
temp_seasonal <- cleaned_dat1 %>%
  filter(site == "midge") %>%
  select(year, season_week, TOBS) %>%
  spread(key = season_week, value = TOBS) %>%
  select(-year)

colnames(temp_seasonal) <- paste("wk", colnames(temp_seasonal), sep = "_")

write.csv(temp_seasonal, "./Datasets/Sunapee/SummarizedData/Midge_year_by_week_airtemp_22JUL19.csv", row.names = FALSE)

#same thing for water temp
watertemp_seasonal <- cleaned_dat1 %>%
  filter(site == "fichter") %>%
  select(year, season_week, watertemp_mean) %>%
  spread(key = season_week, value = watertemp_mean) %>%
  select(-year)

colnames(watertemp_seasonal) <- paste("wk", colnames(watertemp_seasonal), sep = "_")

watertemp_plot <- cleaned_dat1 %>%
  filter(site == "fichter") %>%
  select(year, season_week, watertemp_mean)

ggplot(data = watertemp_plot, aes(x = season_week, y = watertemp_mean, group = year, colour = year))+
  geom_line(size = 1)+
  theme_bw()

write.csv(watertemp_seasonal, "./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_16AUG19.csv", row.names = FALSE)

mean(watertemp_plot$watertemp_mean, na.rm = TRUE)
#21.04
1/var(watertemp_plot$watertemp_mean, na.rm = TRUE)
1/(sd(watertemp_plot$watertemp_mean, na.rm = TRUE)^2)
#0.100


#####################################################################
#cleaning for continuous model
dat <- read_csv("./Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp-Final.csv") %>%
  select(site, date, year, week, totalperL)

years <- c(2007:2017)
years_rep <- rep(years, each = 52)
weeks <- c(1:52)
weeks_rep <- rep(weeks, times = 11)

cont <- tibble(years_rep, weeks_rep) %>%
  mutate(Coffin = "Coffin",
         Fichter = "Fichter",
         Midge = "Midge",
         Newbury = "Newbury") %>%
  gather(Coffin:Newbury, key = "site", value = "id") %>%
  select(-id) %>%
  rename(year = years_rep, week = weeks_rep)

cont2 <- left_join(cont, dat, by = c("site","year","week"))

write.csv(cont2, "./Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp-Continuous_17APR19.csv", row.names = FALSE)

###########################
#cleaning for Schmidt stability

schmidt <- readRDS("~/RProjects/GLEON_Bayesian_WG/Datasets/Sunapee/Stability_metrics/sunapee_schmidt_stability.rds") %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  mutate(date = date(datetime))

dates <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  filter(site == "midge") %>%
  select(date)

schmidt1 <- left_join(dates,schmidt, by = "date") %>%
  group_by(date) %>%
  summarize(schmidt = max(schmidt.stability, na.rm = TRUE))

schmidt1[schmidt1 == -Inf] <- NA
schmidt1[schmidt1 < 0] <- 0

ggplot(data = schmidt1, aes(x = date, y = schmidt))+
  geom_point(size = 2)+
  theme_bw()

schmidt2 <- schmidt1 %>%
  mutate(season_week = rep(c(1:20),times = 6),
         year = year(date)) %>%
  select(year, season_week, schmidt) %>%
  spread(key = season_week, value = schmidt) %>%
  select(-year)

colnames(schmidt2) <- paste("wk", colnames(schmidt2), sep = "_")

mean(schmidt1$schmidt,na.rm = TRUE)
#309.8358

1/(sd(schmidt1$schmidt,na.rm = TRUE)^2)
#3.11e-5

write.csv(schmidt2, "./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_Schmidt_max_16AUG19.csv", row.names = FALSE)


#minimum water temperature

watertemp_seasonal <- cleaned_dat1 %>%
  filter(site == "fichter") %>%
  select(year, season_week, watertemp_min) %>%
  spread(key = season_week, value = watertemp_min) %>%
  select(-year)

colnames(watertemp_seasonal) <- paste("wk", colnames(watertemp_seasonal), sep = "_")

watertemp_plot <- cleaned_dat1 %>%
  filter(site == "fichter") %>%
  select(year, season_week, watertemp_min)

ggplot(data = watertemp_plot, aes(x = season_week, y = watertemp_min, group = year, colour = year))+
  geom_line(size = 1)+
  theme_bw()

write.csv(watertemp_seasonal, "./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv", row.names = FALSE)
