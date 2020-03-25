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


##Gloeo data for Shannon
gl <- cleaned_dat1 %>%
  filter(site == "midge") %>%
  select(site, year, date, season_week, totalperL)

write.csv(gl, "C:/Users/Mary Lofton/Desktop/Gloeo_dates_for_Shannon.csv",row.names = FALSE)



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

write.csv(cleaned_dat1, file = "./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv", row.names = FALSE)

years <- c(2009:2016)

for (i in 1:length(years)){
  check <- cleaned_dat %>% filter(year == years[2])
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

write.csv(gloeo_seasonal, "./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv", row.names = FALSE)

#same thing for air temp
temp_seasonal <- cleaned_dat1 %>%
  filter(site == "midge") %>%
  select(year, season_week, TOBS) %>%
  spread(key = season_week, value = TOBS) %>%
  select(-year)

colnames(temp_seasonal) <- paste("wk", colnames(temp_seasonal), sep = "_")

write.csv(temp_seasonal, "./Datasets/Sunapee/SummarizedData/Midge_year_by_week_airtemp_forecast_05OCT19.csv", row.names = FALSE)

#same thing for water temp
watertemp_seasonal <- cleaned_dat1 %>%
  filter(site == "fichter") %>%
  select(year, season_week, watertemp_min) %>%
  spread(key = season_week, value = watertemp_min)  %>%
  select(-year) 

colnames(watertemp_seasonal) <- paste("wk", colnames(watertemp_seasonal), sep = "_")


write.csv(watertemp_seasonal, "./Datasets/Sunapee/SummarizedData/seasonal_data_mintemp_Fichter_forecast_03MAR20.csv",row.names = FALSE)

colnames(watertemp_seasonal) <- paste("wk", colnames(watertemp_seasonal), sep = "_")

watertemp_plot <- cleaned_dat1 %>%
  filter(site == "midge") %>%
  select(year, season_week, watertemp_min)

ggplot(data = watertemp_plot, aes(x = season_week, y = watertemp_min, group = year, colour = year))+
  geom_line(size = 1)+
  theme_bw()

write.csv(watertemp_seasonal, "./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_forecast_05OCT19.csv", row.names = FALSE)

###########################
#cleaning for max. Schmidt stability

schmidt <- readRDS("~/RProjects/GLEON_Bayesian_WG/Datasets/Sunapee/Stability_metrics/sunapee_schmidt_stability.rds") %>%
  mutate(datetime = as.POSIXct(datetime)) %>%
  mutate(date = date(datetime))

dates <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv") %>%
  filter(site == "midge") %>%
  select(date) 

#####MAX SCHMIDT
schmidt1 <- left_join(dates,schmidt, by = "date") %>%
  group_by(date) %>%
  summarize(schmidt = max(schmidt.stability, na.rm = TRUE))

#########MIN SCHMIDT
schmidt1 <- left_join(dates,schmidt, by = "date") %>%
  group_by(date) %>%
  summarize(schmidt = min(schmidt.stability, na.rm = TRUE))

schmidt1[schmidt1 == -Inf] <- NA
schmidt1[schmidt1 == Inf] <- NA
schmidt1[schmidt1 < 0] <- 0
schmidt1$schmidt[28] <- NA

ggplot(data = schmidt1, aes(x = date, y = schmidt))+
  geom_point(size = 2)+
  theme_bw()

schmidt2 <- schmidt1 %>%
  mutate(season_week = rep(c(1:20),times = 8),
         year = year(date)) %>%
  select(year, season_week, schmidt) %>%
  spread(key = season_week, value = schmidt) %>%
  select(-year)

colnames(schmidt2) <- paste("wk", colnames(schmidt2), sep = "_")

mean(schmidt1$schmidt,na.rm = TRUE)
#309.8358

1/(sd(schmidt1$schmidt,na.rm = TRUE)^2)
#3.11e-5

write.csv(schmidt2, "./Datasets/Sunapee/SummarizedData/seasonal_data_minSchmidt_05MAR20.csv", row.names = FALSE)

##precip
precip <- read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/gloeo_Midge_airtemp_precip.csv")
raw_precip <- read_csv("./Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)
sampling_dates1 <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  mutate(Date = ymd(date)) %>%
  select(Date) 
sampling_dates2 <- unique(sampling_dates1)
sampling_dates <- sampling_dates2[-38,1] %>%
  add_column(sampling_time = 1:120)
head(sampling_dates)
head(raw_precip)

check <- left_join(raw_precip, sampling_dates) 


for(i in 1:nrow(check)){
if(!is.na(check$sampling_time[i])){
  check$sampling_time[c((i-6):(i-1))] <- rep(check$sampling_time[i],6)
}
}

final <- subset(check, !is.na(sampling_time))
colnames(final)[6]<-"sampling_time_temp"
final2 <- left_join(final, sampling_dates, by = "Date")

#because there are uneven numbers of days between sampling dates sometimes,
#going to do manual editing of file to be sure numbers line up
write.csv(final2,"./Datasets/Sunapee/SummarizedData/midge_weekly_precip_temp.csv",row.names = FALSE)
821/120
120*7

#read back in edited file
prec <- read_csv("./Datasets/Sunapee/SummarizedData/midge_weekly_precip_temp.csv") %>%
  select(-sampling_time) %>%
  rename(sampling_time = sampling_time_temp) %>%
  group_by(sampling_time)%>%
  summarize(ppt_weekly_sum_mm = sum(ppt_mm))

prec_final <- left_join(prec,sampling_dates,by = 'sampling_time')

prec_final2 <- prec_final %>%
  mutate(season_week = rep(c(1:20),times = 6)) %>%
  mutate(sampling_year = ifelse(sampling_time %in% c(1:20),2009,
                                ifelse(sampling_time %in% c(21:40),2010,
                                       ifelse(sampling_time %in% c(41:60),2011,
                                              ifelse(sampling_time %in% c(61:80),2012,
                                                     ifelse(sampling_time %in% c(81:100),2013,2014)))))) %>%
  select(season_week, ppt_weekly_sum_mm, sampling_year) 

ggplot(data = prec_final2, aes(x = season_week,y= ppt_weekly_sum_mm,col = as.factor(sampling_year)))+
  geom_line(size = 1)+
  theme_bw()

prec_final3 <- prec_final2 %>%
  spread(key = season_week, value = ppt_weekly_sum_mm) %>%
  select(-sampling_year)

colnames(prec_final3) <- paste("wk", colnames(prec_final3), sep = "_")
week_avg = colMeans(prec_final3, na.rm = TRUE)
week_var_mean = mean(1/apply(prec_final3,2,var),na.rm = TRUE)
week_var_var = var(1/apply(prec_final3,2,var),na.rm = TRUE)


write.csv(prec_final3, "./Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_summed_precip_10OCT19.csv", row.names = FALSE)




#############################BUOY PAR DATA########################
par <- read_csv("./Datasets/Sunapee/RawData/Sunapee buoy data/met_data/2007-2017_PAR_L1.csv") %>%
  filter(location == "loon") %>%
  mutate(Date = date(datetime)) %>%
  group_by(Date)%>%
  summarize(daily_mean_PAR_umolm2s = mean(PAR_umolm2s, na.rm = TRUE))
head(par)

sampling_dates1 <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  mutate(Date = ymd(date)) %>%
  select(Date) 
sampling_dates2 <- unique(sampling_dates1)
sampling_dates <- sampling_dates2[-38,1] %>%
  add_column(sampling_time = 1:120)

check <- left_join(par, sampling_dates) 


for(i in 1:nrow(check)){
  if(!is.na(check$sampling_time[i])){
    check$sampling_time[c((i-6):(i-1))] <- rep(check$sampling_time[i],6)
  }
}

final <- subset(check, !is.na(sampling_time))
colnames(final)[3]<-"sampling_time_temp"
final2 <- left_join(final, sampling_dates, by = "Date")

#because there are uneven numbers of days between sampling dates sometimes,
#going to do manual editing of file to be sure numbers line up
write.csv(final2,"./Datasets/Sunapee/SummarizedData/midge_weekly_PAR_temp.csv",row.names = FALSE)

parr <- read_csv("./Datasets/Sunapee/SummarizedData/midge_weekly_PAR_temp.csv") %>%
  select(-sampling_time) %>%
  rename(sampling_time = sampling_time_temp) %>%
  group_by(sampling_time)%>%
  summarize(weekly_mean_PAR_umolm2s = mean(daily_mean_PAR_umolm2s, na.rm = TRUE))

par_final <- left_join(sampling_dates,parr,by = 'sampling_time') %>%
  mutate(weekly_mean_PAR_umolm2s = ifelse(weekly_mean_PAR_umolm2s=="NaN",NA,weekly_mean_PAR_umolm2s))

par_final2 <- par_final %>%
  mutate(season_week = rep(c(1:20),times = 6)) %>%
  mutate(sampling_year = ifelse(sampling_time %in% c(1:20),2009,
                                ifelse(sampling_time %in% c(21:40),2010,
                                       ifelse(sampling_time %in% c(41:60),2011,
                                              ifelse(sampling_time %in% c(61:80),2012,
                                                     ifelse(sampling_time %in% c(81:100),2013,2014)))))) %>%
  select(season_week, weekly_mean_PAR_umolm2s, sampling_year) 

ggplot(data = par_final2, aes(x = season_week,y= weekly_mean_PAR_umolm2s,col = as.factor(sampling_year)))+
  geom_line(size = 1)+
  theme_bw()

par_final3 <- par_final2 %>%
  spread(key = season_week, value = weekly_mean_PAR_umolm2s) %>%
  select(-sampling_year)

colnames(par_final3) <- paste("wk", colnames(par_final3), sep = "_")
week_avg = colMeans(par_final3, na.rm = TRUE)
week_var_mean = mean(1/apply(par_final3,2,var, na.rm = TRUE),na.rm = TRUE)
week_var_var = var(1/apply(par_final3,2,var, na.rm = TRUE),na.rm = TRUE)


write.csv(par_final3, "./Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_mean_buoyPAR_12OCT19.csv", row.names = FALSE)

apply(par_final3,2,var, na.rm = TRUE)

###############WINDSPEED#########################
wnd <- read_csv("./Datasets/Sunapee/RawData/Sunapee buoy data/met_data/2007-2017_wind_L1.csv") %>%
  filter(location == "loon") %>%
  mutate(Date = date(datetime)) #%>%
#   group_by(Date)%>%
#   summarize(daily_mean_wnd_ms = mean(WindSp_ms, na.rm = TRUE))
# head(wnd)

sampling_dates1 <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  mutate(Date = ymd(date)) %>%
  select(Date) 
sampling_dates2 <- unique(sampling_dates1)
sampling_dates <- sampling_dates2[-38,1] %>%
  add_column(sampling_time = 1:120)

check <- left_join(wnd, sampling_dates) 


for(i in 1:nrow(check)){
  if(!is.na(check$sampling_time[i])){
    check$sampling_time[c((i-(36*6)):(i-1))] <- rep(check$sampling_time[i],36*6)
  }
}

final <- subset(check, !is.na(sampling_time)) %>%
  filter(Date >= "2009-05-01") %>%
   group_by(sampling_time)%>%
   summarize(perc_90_wnd_ms = quantile(WindSp_ms, probs = 0.9,na.rm = TRUE,names = FALSE))

wnd_final <- left_join(sampling_dates,final,by = 'sampling_time') 

wnd_final2 <- wnd_final %>%
  mutate(season_week = rep(c(1:20),times = 6)) %>%
  mutate(sampling_year = ifelse(sampling_time %in% c(1:20),2009,
                                ifelse(sampling_time %in% c(21:40),2010,
                                       ifelse(sampling_time %in% c(41:60),2011,
                                              ifelse(sampling_time %in% c(61:80),2012,
                                                     ifelse(sampling_time %in% c(81:100),2013,2014)))))) %>%
  select(season_week, perc_90_wnd_ms, sampling_year) 

ggplot(data = wnd_final2, aes(x = season_week,y= perc_90_wnd_ms,col = as.factor(sampling_year)))+
  geom_line(size = 1)+
  theme_bw()

wnd_final3 <- wnd_final2 %>%
  spread(key = season_week, value = perc_90_wnd_ms) %>%
  select(-sampling_year)

colnames(wnd_final3) <- paste("wk", colnames(wnd_final3), sep = "_")
week_avg = colMeans(wnd_final3, na.rm = TRUE)
week_var_mean = mean(1/apply(wnd_final3,2,var, na.rm = TRUE),na.rm = TRUE)
week_var_var = var(1/apply(wnd_final3,2,var, na.rm = TRUE),na.rm = TRUE)


write.csv(wnd_final3, "./Datasets/Sunapee/Bayes_Covariates_Data/midge_wind_perc90_14OCT19.csv", row.names = FALSE)
check <- read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_wind_perc90_14OCT19.csv")

################GROWING DEGREE DAYS
gdd <- cleaned_dat1 %>%
  filter(!date %in% bad_dates) %>%
  arrange(year,date,site) %>%
  mutate(season_week = rep(c(1:20),times = 8, each = 4))

gdd1 <- gdd %>%
  filter(site == "midge") %>%
  select(year, date, season_week, watertemp_max, watertemp_min) %>%
  mutate(watertemp_gdd = ((watertemp_max + watertemp_min)/2) - 4) %>%
  mutate(watertemp_gdd = ifelse(is.na(watertemp_gdd),0,watertemp_gdd))%>%
  select(-watertemp_max,-watertemp_min,-date) %>%
  spread(key = year, value = watertemp_gdd) %>%
  select(-season_week)

#write.csv(gdd1, "./Datasets/Sunapee/SummarizedData/midge_cumulativeGDD_27FEB20.csv", row.names = FALSE)

for (i in 1:ncol(gdd1)){
  gdd1[,i] <- cumsum(na.omit(gdd1[,i]))
}

gdd1[3,1] <- NA
gdd1[19,1] <- NA

gdd2 <- as.tibble(t(gdd1))

colnames(gdd2) <- colnames(gloeo_seasonal)

write.csv(gdd2, "./Datasets/Sunapee/SummarizedData/GDD_year_by_week_forecast_03MAR20.csv", row.names = FALSE)

gdd_reg <- as.numeric(c(gdd2[1,],gdd2[2,],gdd2[3,],gdd2[4,],gdd2[5,],gdd2[6,]))

gloeo_seasonal_reg <- c(gloeo_seasonal[1,],gloeo_seasonal[2,],gloeo_seasonal[3,],gloeo_seasonal[4,],gloeo_seasonal[5,],gloeo_seasonal[6,])
gloeo_seasonal_reg2 <- log(as.numeric(gloeo_seasonal_reg)+0.003)

plot(gdd_reg,gloeo_seasonal_reg2)

summary(lm(gloeo_seasonal_reg2~gdd_reg))$r.squared

gdd_reg2 <- gdd_reg^2

quadratic.model <-lm(gloeo_seasonal_reg2 ~ gdd_reg + gdd_reg2)

summary(quadratic.model)

gdd_values <- seq(0, 400, 1)

predicted.gloeo <- predict(quadratic.model,list(gdd_reg=gdd_values, gdd_reg2=gdd_values^2))

plot(gdd_reg,gloeo_seasonal_reg2)
lines(gdd_values, predicted.gloeo, col = "darkgreen", lwd = 3)

for (i in 1:nrow(gdd2)){
gdd3 <- as.numeric(gdd2[i,])
gloeo <- log(as.numeric(gloeo_seasonal[i,])+0.003)

gdd4 <- gdd3^2

quadratic.model <-lm(gloeo ~ gdd3 + gdd4)

print(summary(quadratic.model)$r.squared)

gdd_values <- seq(0, 400, 1)

predicted.gloeo <- predict(quadratic.model,list(gdd3=gdd_values, gdd4=gdd_values^2))

plot(gdd3,gloeo)
lines(gdd_values, predicted.gloeo, col = "darkgreen", lwd = 3)}

################DAY LENGTH
gdd <- cleaned_dat1 %>%
  filter(!date %in% bad_dates) %>%
  arrange(year,date,site) %>%
  mutate(season_week = rep(c(1:20),times = 8, each = 4)) %>%
  filter(site == "midge") %>%
  select(year, date, season_week, daylength)

write.csv(gdd, "./Datasets/Sunapee/SummarizedData/seasonal_data_daylength_forecast.csv", row.names = FALSE)


gdd2 <- gdd %>%
  filter(site == "midge") %>%
  select(year, season_week, daylength) %>%
  spread(key = season_week, value = daylength) %>%
  select(-year)

write.csv(gdd2, "./Datasets/Sunapee/SummarizedData/daylength_year_by_week_forecast_09FEB20.csv", row.names = FALSE)

gdd_reg <- as.numeric(c(gdd2[1,],gdd2[2,],gdd2[3,],gdd2[4,],gdd2[5,],gdd2[6,]))

gloeo_seasonal_reg <- c(gloeo_seasonal[1,],gloeo_seasonal[2,],gloeo_seasonal[3,],gloeo_seasonal[4,],gloeo_seasonal[5,],gloeo_seasonal[6,])
gloeo_seasonal_reg2 <- log(as.numeric(gloeo_seasonal_reg)+0.003)

plot(gdd_reg,gloeo_seasonal_reg2)

summary(lm(gloeo_seasonal_reg2~gdd_reg))$r.squared

gdd_reg2 <- gdd_reg^2

quadratic.model <-lm(gloeo_seasonal_reg2 ~ gdd_reg + gdd_reg2)

summary(quadratic.model)

gdd_values <- seq(10, 16, 0.1)

predicted.gloeo <- predict(quadratic.model,list(gdd_reg=gdd_values, gdd_reg2=gdd_values^2))

plot(gdd_reg,gloeo_seasonal_reg2)
lines(gdd_values, predicted.gloeo, col = "darkgreen", lwd = 3)

for (i in 1:nrow(gdd2)){
  gdd3 <- as.numeric(gdd2[i,])
  gloeo <- log(as.numeric(gloeo_seasonal[i,])+0.003)
  
  gdd4 <- gdd3^2
  
  quadratic.model <-lm(gloeo ~ gdd3 + gdd4)
  
  print(summary(quadratic.model)$r.squared)
  
  gdd_values <- seq(0, 400, 1)
  
  predicted.gloeo <- predict(quadratic.model,list(gdd3=gdd_values, gdd4=gdd_values^2))
  
  plot(gdd3,gloeo)
  lines(gdd_values, predicted.gloeo, col = "darkgreen", lwd = 3)}

################OTHER VARS
gdd <- cleaned_dat1 %>%
  filter(!date %in% bad_dates) %>%
  arrange(year,date,site) %>%
  mutate(season_week = rep(c(1:20),times = 6, each = 4))

gdd2 <- gdd %>%
  filter(site == "midge") %>%
  select(year, season_week, watertemp_max) %>%
  spread(key = season_week, value = watertemp_max) %>%
  select(-year)

gdd2 <- read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv")

gdd_reg <- as.numeric(c(gdd2[1,],gdd2[2,],gdd2[3,],gdd2[4,],gdd2[5,],gdd2[6,]))

gloeo_seasonal_reg <- c(gloeo_seasonal[1,],gloeo_seasonal[2,],gloeo_seasonal[3,],gloeo_seasonal[4,],gloeo_seasonal[5,],gloeo_seasonal[6,])
gloeo_seasonal_reg2 <- log(as.numeric(gloeo_seasonal_reg)+0.003)

plot(gdd_reg,gloeo_seasonal_reg2)

summary(lm(gloeo_seasonal_reg2~gdd_reg))$r.squared

gdd_reg2 <- gdd_reg^2

quadratic.model <-lm(gloeo_seasonal_reg2 ~ gdd_reg + gdd_reg2)

summary(quadratic.model)

gdd_values <- seq(10, 16, 0.1)

predicted.gloeo <- predict(quadratic.model,list(gdd_reg=gdd_values, gdd_reg2=gdd_values^2))

plot(gdd_reg,gloeo_seasonal_reg2)
lines(gdd_values, predicted.gloeo, col = "darkgreen", lwd = 3)

for (i in 1:nrow(gdd2)){
  gdd3 <- as.numeric(gdd2[i,])
  gloeo <- log(as.numeric(gloeo_seasonal[i,])+0.003)
  
  gdd4 <- gdd3^2
  
  linear.model <-lm(gloeo ~ gdd3 + gdd4)
  
  print(summary(linear.model)$r.squared)
  
  quadratic.model <-lm(gloeo ~ gdd3 + gdd4)
  
  print(summary(quadratic.model)$r.squared)
  
  gdd_values <- seq(0, 700, 1)
  
  predicted.gloeo <- predict(quadratic.model,list(gdd3=gdd_values, gdd4=gdd_values^2))
  
  plot(gdd3,gloeo)
  lines(gdd_values, predicted.gloeo, col = "darkgreen", lwd = 3)}

########################HOBO PAR DATA
hobo <- read_csv("./Datasets/Sunapee/SummarizedData/light_day_HOBO_aggregation.csv") %>%
  select(date, light_Newbury.sum)

head(hobo)

datez <- cleaned_dat1 %>%
  filter(site == "newbury") %>%
  select(date, year, season_week)

check <- left_join(datez, hobo, by = "date")

#get in shape for seasonal for-loop
hobo_seasonal <- check %>%
  select(year, season_week, light_Newbury.sum) %>%
  spread(key = season_week, value = light_Newbury.sum) %>%
  select(-year)

colnames(hobo_seasonal) <- paste("wk", colnames(hobo_seasonal), sep = "_")

write.csv(hobo_seasonal, "./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv", row.names = FALSE)

