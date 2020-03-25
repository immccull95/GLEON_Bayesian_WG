# Testing Covariates Code
# Created by JAB 7 Oct 2019

# Load Libraries ####
library(devtools)
library(plyr)
library(tidyverse)
library(lubridate)

# Linear model function to text annotation ####
lm_labels <- function(dat) {
  mod <- lm(onset_wtr_temp ~ TempC_1p5m_mean, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$TempC_1p5m_mean, dat$onset_wtr_temp)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(onset_buoy, "site", lm_labels)
labels

# Water Temp Daily Summaries ####

# Load Midge gloeo + water temp data
gloeo_Midge_watertemp_day <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_day_14Aug2019.csv")

# shorten data to match model season - weeks 21-40 last week of May to 1st week of Oct
gloeo_Midge_watertemp_day_season <- gloeo_Midge_watertemp_day %>% 
  filter(20 < week, week < 41)

gloeo_Midge_noNA <- gloeo_Midge_watertemp_day_season %>% 
  filter(watertemp_mean_interp!="NA") 
#%>% filter(totalperL != 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ watertemp_mean_interp, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$watertemp_mean_interp, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$watertemp_daily_range

scale_x <- max(x)-2.5
scale_y <- min(y)+0.5

scale_rx <- max(x)-2
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=watertemp_daily_range,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Range Water Temp",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

#
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Mean_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Median_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Min_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Max_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_SD_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Range_Wtr_Temp.pdf", width = 13, height = 8.5)

# Water Temp 1 week Lag ####
# Real 1 week lag
gloeo_Midge_1week_lag <- gloeo_Midge_watertemp_day %>% 
  mutate(date_1weeklag = date - dweeks(1)) %>% 
  select(date, log_gloeo, date_1weeklag)

onset_watertemp_day <-  read_csv("Datasets/Sunapee/SummarizedData/Onset_watertemp_day_long_14Aug2019.csv")

onset_watertemp_day_Midge <- onset_watertemp_day %>% 
  filter(site=="midge") %>% 
  rename(date_1weeklag = date)

#Join water temp with 1 week lag date
join_gloeo_Midge_1week_lag <- left_join(gloeo_Midge_1week_lag, onset_watertemp_day_Midge,by = "date_1weeklag")

join_gloeo_Midge_1week_lag_yr <- join_gloeo_Midge_1week_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

write_csv(join_gloeo_Midge_1week_lag_yr, "Datasets/Sunapee/SummarizedData/join_gloeo_Midge_1week_lag-real.csv")

# Water Temp Lag Plots ####

# shorten data to match model season - weeks 21-40 last week of May to 1st week of Oct
gloeo_Midge_watertemp_day_season <- join_gloeo_Midge_1week_lag_yr %>% 
  filter(20 < week, week < 41)

gloeo_Midge_noNA <- gloeo_Midge_watertemp_day_season %>% 
  filter(watertemp_daily_range!="NA") 
#%>% filter(totalperL != 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ watertemp_daily_range, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$watertemp_daily_range, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$watertemp_daily_range

scale_x <- max(x)-2
scale_y <- min(y)+0.5

scale_rx <- max(x)-1.5
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=watertemp_daily_range,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Week Lag Range Water Temp",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

#
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Lag_Mean_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Lag_Median_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Lag_Min_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Lag_Max_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Lag_SD_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Lag_Range_Wtr_Temp.pdf", width = 13, height = 8.5)



# Water Temp 1 week Diff ####
onset_watertemp_day_Midge <- onset_watertemp_day %>% 
  filter(site=="midge") %>% 
  mutate(year = year(date)) %>% 
  filter(year==2016)

# calculate week to week difference in water temp - mean,min, max, sd, range
wtr_mean_diff <- vector("double", nrow(onset_watertemp_day_Midge))  # 1. output
wtr_median_diff <- vector("double", nrow(onset_watertemp_day_Midge))  # 1. output
wtr_max_diff <- vector("double", nrow(onset_watertemp_day_Midge))  # 1. output
wtr_min_diff <- vector("double", nrow(onset_watertemp_day_Midge))  # 1. output
wtr_sd_diff <- vector("double", nrow(onset_watertemp_day_Midge))  # 1. output
wtr_range_diff <- vector("double", nrow(onset_watertemp_day_Midge))  # 1. output

for (i in 1:nrow(onset_watertemp_day_Midge)) {
  wtr_mean_diff[i] <- onset_watertemp_day_Midge$watertemp_daily_mean[i+7] - onset_watertemp_day_Midge$watertemp_daily_mean[i]
  wtr_median_diff[i] <- onset_watertemp_day_Midge$watertemp_daily_median[i+7] - onset_watertemp_day_Midge$watertemp_daily_median[i]
  wtr_max_diff[i] <- onset_watertemp_day_Midge$watertemp_daily_max[i+7] - onset_watertemp_day_Midge$watertemp_daily_max[i]
  wtr_min_diff[i] <- onset_watertemp_day_Midge$watertemp_daily_min[i+7] - onset_watertemp_day_Midge$watertemp_daily_min[i]
  wtr_sd_diff[i] <- onset_watertemp_day_Midge$watertemp_daily_sd[i+7] - onset_watertemp_day_Midge$watertemp_daily_sd[i]
  wtr_range_diff[i] <- onset_watertemp_day_Midge$watertemp_daily_range[i+7] - onset_watertemp_day_Midge$watertemp_daily_range[i]
  
  wtr_diff_output <- data.frame(wtr_mean_diff = wtr_mean_diff, wtr_median_diff = wtr_median_diff, wtr_max_diff = wtr_max_diff, wtr_min_diff = wtr_min_diff, wtr_sd_diff = wtr_sd_diff, wtr_range_diff = wtr_range_diff)
  }

wtr_diff_output_2016 <- cbind(date = onset_watertemp_day_Midge[-c(1:7),]$date, wtr_diff_output[1:114,])

wtr_diff_output_all <- bind_rows(wtr_diff_output_2006, wtr_diff_output_2007, wtr_diff_output_2008, wtr_diff_output_2009, wtr_diff_output_2010, wtr_diff_output_2011, wtr_diff_output_2012, wtr_diff_output_2013, wtr_diff_output_2014, wtr_diff_output_2015, wtr_diff_output_2016)

write_csv(wtr_diff_output_all, "~/Desktop/wtr_diff_output_all.csv")

# Join with gloeo data
gloeo_Midge_wtr_diff <- left_join(gloeo_Midge_watertemp_day,wtr_diff_output_all,by="date")

write_csv(gloeo_Midge_wtr_diff,"Datasets/Sunapee/SummarizedData/gloeo_Midge_wtr_diff.csv")

# Water Diff Plot ####

gloeo_Midge_wtr_diff_final = read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_wtr_diff.csv")

# shorten data to match model season - weeks 21-40 last week of May to 1st week of Oct
gloeo_Midge_watertemp_day_season <- gloeo_Midge_wtr_diff_final %>% 
  filter(20 < week, week < 41)

gloeo_Midge_noNA <- gloeo_Midge_watertemp_day_season %>% 
  filter(wtr_range_diff!="NA") 
#%>% filter(totalperL != 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ wtr_range_diff, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$wtr_range_diff, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$wtr_range_diff

scale_x <- max(x)-3
scale_y <- min(y)+0.5

scale_rx <- max(x)-1.5
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=wtr_range_diff,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Week Difference in Range Water Temp",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

#
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Diff_Mean_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Diff_Median_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Diff_Min_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Diff_Max_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Diff_SD_Wtr_Temp.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Diff_Range_Wtr_Temp.pdf", width = 13, height = 8.5)

# Growing Degree Days Calc ####

# set base temp to 4°C, ag plants often use 10°C
base_temp <- 4

GDD <- gloeo_Midge_watertemp_day %>% 
  mutate(gdd = (watertemp_daily_max + watertemp_daily_min/2) - base_temp)

write_csv(GDD,"Datasets/Sunapee/SummarizedData/midge_gloeo_GDD.csv")

# GDD Plot ####
# shorten data to match model season - weeks 21-40 last week of May to 1st week of Oct
gloeo_Midge_watertemp_day_season <- GDD %>% 
  filter(20 < week, week < 41)

plot(gdd ~ date, data = gloeo_Midge_watertemp_day_season)
lines(gloeo_Midge_watertemp_day_season$watertemp_daily_mean ~ gloeo_Midge_watertemp_day_season$date, col = "red")

gloeo_Midge_noNA <- gloeo_Midge_watertemp_day_season %>% 
  filter(gdd!="NA") 
#%>% filter(totalperL != 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ gdd, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$gdd, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$gdd

scale_x <- max(x)-9
scale_y <- min(y)+0.5

scale_rx <- max(x)-5
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=gdd,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="GDD",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

#
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_GDD.pdf", width = 13, height = 8.5)


# Moving avg for water temp ####

library(zoo)

# Read in initial Onset hourly data
onset_water_temp <- read_csv("Datasets/Sunapee/SummarizedData/Onset_wtrtemp_60min_2006-2016_Allsites.csv",col_types = cols(
  coffin = col_double(),
  fichter = col_double(),
  newbury = col_double()))

# 2009 data - readings every 30 min so filtered out to only include hourly readings
onset_watertemp_hourly <- onset_water_temp %>% 
  mutate(minute = minute(datetime)) %>% 
  filter(minute == 0) %>% 
  select(-minute)

# Convert to long and filter for Midge
midge_onset_water_temp_long <- onset_watertemp_hourly %>% 
  gather(key="site",value="onset_wtr_temp",c(coffin,fichter,newbury,midge)) %>% 
  filter(site=="midge") %>% 
  filter(year==2016) 

ma_2016 <- midge_onset_water_temp_long[-c(1:6),c(3,7)] %>% #[-c(1:6),]
  mutate(ma_3 = rollmean(onset_wtr_temp, k = 72, fill = NA, align = "right")) %>% 
  mutate(ma_5 = rollmean(onset_wtr_temp, k = 120, fill = NA, align = "right")) %>% 
  mutate(ma_7 = rollmean(onset_wtr_temp, k = 168, fill = NA, align = "right")) %>% 
  mutate(ma_10 = rollmean(onset_wtr_temp, k = 240, fill = NA, align = "right")) %>% 
  mutate(ma_14 = rollmean(onset_wtr_temp, k = 336, fill = NA, align = "right")) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean), na.rm = T)

ma_all <- bind_rows(ma_2006, ma_2007, ma_2008, ma_2009, ma_2010, ma_2011, ma_2012, ma_2013, ma_2014, ma_2015, ma_2016)

write_csv(ma_all,"Datasets/Sunapee/SummarizedData/watertemp_movingavg_dailymean.csv")

plot(onset_wtr_temp ~ datetime, data = midge_watertemp_ma)
lines(ma_7 ~ datetime, data = midge_watertemp_ma, col = "red")
lines(ma_5 ~ datetime, data = midge_watertemp_ma, col = "blue")
lines(ma_3 ~ datetime, data = midge_watertemp_ma, col = "yellow")

#Join ma_all with gloeo
ma_all2 <- ma_all[,-2]

gloeo_Midge_moving_avg <- left_join(gloeo_Midge_watertemp_day,ma_all2,by="date")

write_csv(gloeo_Midge_moving_avg,"Datasets/Sunapee/SummarizedData/gloeo_Midge_moving_avg.csv")

# Plot Moving Avg ####
# shorten data to match model season - weeks 21-40 last week of May to 1st week of Oct
gloeo_Midge_watertemp_day_season <- gloeo_Midge_moving_avg %>% 
  filter(20 < week, week < 41)

gloeo_Midge_noNA <- gloeo_Midge_watertemp_day_season %>% 
  filter(ma_14!="NA") %>% 
  filter(ma_14!="NaN")
#%>% filter(totalperL != 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ ma_14, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$ma_14, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$ma_14

scale_x <- max(x)-6
scale_y <- min(y)+0.5

scale_rx <- max(x)-5
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=ma_14,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="14 day Water Temp Moving Avg.",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

#
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MA3.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MA5.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MA7.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MA10.pdf", width = 13, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MA14.pdf", width = 13, height = 8.5)

# Schmidt Stability daily summaries ####
gloeo_schmidt_Midge <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_schmidt_Midge_14Aug2019.csv")

gloeo_Midge_season <- gloeo_schmidt_Midge %>% 
  filter(20 < week, week < 41)

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(schmidt.stability_range!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ schmidt.stability_range, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$schmidt.stability_range)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$schmidt.stability_sd

scale_x <- max(x)-300
scale_y <- min(y)+0.5

scale_rx <- max(x)-200
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=schmidt.stability_max,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Max Daily Schmidt Stability",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MeanSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MedianSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MaxSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_MinSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_RangeSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_SDSchmidt_Midge.jpeg", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Diff_MinSchmidt_Midge.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Diff_MeanSchmidt_Midge.jpeg", width = 11, height = 8.5)

# Schmidt - 1 week Lag ####
gloeo_schmidt_Midge <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_schmidt_Midge_14Aug2019.csv")

gloeo_schmidt_Midge_1week_lag <- gloeo_schmidt_Midge %>% 
  mutate(date_1weeklag = date - dweeks(1)) %>% 
  select(date, log_gloeo, date_1weeklag)

schmidt_daily <- read_csv("Datasets/Sunapee/SummarizedData/schmidt_stability_daily_summary.csv")

schmidt_daily_lag <- schmidt_daily %>% 
  rename(date_1weeklag = date)

#Join water temp with 1 week lag date
join_gloeo_Midge_1week_lag <- left_join(gloeo_schmidt_Midge_1week_lag, schmidt_daily_lag,by = "date_1weeklag")

join_gloeo_Midge_1week_lag_yr <- join_gloeo_Midge_1week_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

# Write Schmidt 1 week lag
write_csv(join_gloeo_Midge_1week_lag_yr, "Datasets/Sunapee/SummarizedData/Midge_gloeo_schmidt_1weekLAG.csv")

# Schmidt Lag Plots ####

gloeo_Midge_season <- join_gloeo_Midge_1week_lag_yr %>% 
  filter(20 < week, week < 41)

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(schmidt.stability.range!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ schmidt.stability.range, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$schmidt.stability.range)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$schmidt.stability.max

scale_x <- max(x)-300
scale_y <- min(y)+0.5

scale_rx <- max(x)-200
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=schmidt.stability.max,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Week Lag Max Daily Schmidt Stability",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_MeanSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MedianSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MaxSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MinSchmidt_Midge-1weeklag.jpeg", width = 11, height = 8.5)



# Schmidt - 1 week Diff ####

# Read in daily schmidt data

schmidt_daily <- read_csv("Datasets/Sunapee/SummarizedData/schmidt_stability_daily_summary.csv")

#Filter for each year
schmidt_year <- schmidt_daily %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  filter(year==2014) %>% 
  filter(month %in% c(4:10))

# calculate week to week difference in water temp - mean,min, max, sd, range
schmidt_mean_diff <- vector("double", nrow(schmidt_year))  # 1. output
schmidt_median_diff <- vector("double", nrow(schmidt_year))  # 1. output
schmidt_max_diff <- vector("double", nrow(schmidt_year))  # 1. output
schmidt_min_diff <- vector("double", nrow(schmidt_year))  # 1. output
schmidt_sd_diff <- vector("double", nrow(schmidt_year))  # 1. output
schmidt_range_diff <- vector("double", nrow(schmidt_year))  # 1. output

for (i in 1:nrow(schmidt_year)) {
  schmidt_mean_diff[i] <- schmidt_year$schmidt.stability.mean[i+7] - schmidt_year$schmidt.stability.mean[i]
  schmidt_median_diff[i] <- schmidt_year$schmidt.stability.median[i+7] - schmidt_year$schmidt.stability.median[i]
  schmidt_max_diff[i] <- schmidt_year$schmidt.stability.max[i+7] - schmidt_year$schmidt.stability.max[i]
  schmidt_min_diff[i] <- schmidt_year$schmidt.stability.min[i+7] - schmidt_year$schmidt.stability.min[i]
  schmidt_sd_diff[i] <- schmidt_year$schmidt.stability.sd[i+7] - schmidt_year$schmidt.stability.sd[i]
  schmidt_range_diff[i] <- schmidt_year$schmidt.stability.range[i+7] - schmidt_year$schmidt.stability.range[i]
  
  schmidt_diff_output <- data.frame(schmidt_mean_diff = schmidt_mean_diff, schmidt_median_diff = schmidt_median_diff, schmidt_max_diff = schmidt_max_diff, schmidt_min_diff = schmidt_min_diff, schmidt_sd_diff = schmidt_sd_diff, schmidt_range_diff = schmidt_range_diff)
}

schmidt_diff_output_2014 <- cbind(date = schmidt_year[-c(1:7),]$date, schmidt_diff_output[1:138,])

schmidt_diff_output_all <- bind_rows(schmidt_diff_output_2007, schmidt_diff_output_2008, schmidt_diff_output_2009, schmidt_diff_output_2010, schmidt_diff_output_2011, schmidt_diff_output_2012, schmidt_diff_output_2013, schmidt_diff_output_2014, schmidt_diff_output_2015, schmidt_diff_output_2016)

write_csv(schmidt_diff_output_all, "Datasets/Sunapee/SummarizedData/schmidt_diff_output_all.csv")

# Join with gloeo data
gloeo_Midge_schmidt_diff <- left_join(gloeo_schmidt_Midge,schmidt_diff_output_all,by="date")

write_csv(gloeo_Midge_schmidt_diff,"Datasets/Sunapee/SummarizedData/gloeo_Midge_schmidt_diff.csv")

# Filter for season
gloeo_Midge_schmidt_diff_season <- gloeo_Midge_schmidt_diff %>%
  mutate(week = week(date)) %>% 
  filter(20 < week, week < 41) 

write_csv(gloeo_Midge_schmidt_diff_season,"Datasets/Sunapee/SummarizedData/gloeo_Midge_schmidt_diff_season.csv")


# Schmidt Diff Plots ####

# Filter for season
gloeo_Midge_schmidt_diff_season <- gloeo_Midge_schmidt_diff %>%
  mutate(week = week(date)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_schmidt_diff_season %>% 
  filter(schmidt_range_diff!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ schmidt_range_diff, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$schmidt_range_diff)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$schmidt_sd_diff

scale_x <- max(x)-220
scale_y <- min(y)

scale_rx <- max(x)-150
scale_ry <- min(y)+0.5

ggplot(gloeo_Midge_noNA, aes(x=schmidt_sd_diff,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Week Diff SD Daily Schmidt Stability",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_MeanSchmidt_Midge-1weekDiff.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MedianSchmidt_Midge-1weekDiff.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MinSchmidt_Midge-1weekDiff.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_MaxSchmidt_Midge-1weekDiff.jpeg", width = 11, height = 8.5)


# Underwater light ####

gloeo_Midge_watertemp_day <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_watertemp_day_14Aug2019.csv")

# shorten data to match model season - weeks 21-40 last week of May to 1st week of Oct
gloeo_Midge_watertemp_day_season <- gloeo_Midge_watertemp_day %>% 
  filter(20 < week, week < 41) %>% 
  filter(year==2013)

plot(light_sum ~ date, data = gloeo_Midge_watertemp_day_season)

gloeo_Midge_noNA <- gloeo_Midge_watertemp_day_season %>% 
  filter(light_sum!="NA") 
#%>% filter(totalperL != 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ light_sum, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$light_sum, dat$log_gloeo)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$light_sum

scale_x <- max(x)-200000
scale_y <- min(y)+0.5

scale_rx <- max(x)-200000
scale_ry <- min(y)

ggplot(gloeo_Midge_noNA, aes(x=light_sum,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Daily Sum Underwater Light (Lux)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = 1, y= -3, aes(label = formula), data = labels, parse = T, hjust = 0)+
  #geom_text(x = 3, y= -3.75, aes(label = r2), data = labels, parse = T, hjust = 0)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

#
ggsave("~/Desktop/Gloeo Plots/Final_Covariates/LogGloeo_Sum_Light_Lux.pdf", width = 13, height = 8.5)


# Median Surface Density vs. Aug Min Air Temp & annual sum precip ####
#Daily Air Temp
airtemp_daily <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

# Annual sum of precip
annual_sum_precip <- airtemp_daily %>% #[1:13514,]
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  filter(year > 2004 & year < 2017) %>% 
  group_by(year) %>% 
  summarize(sum_precip = sum(ppt_mm))

#Join with median gloeo
gloeo_Midge_annual_precip <- full_join(gloeo_Midge_surface_median,annual_sum_precip,by="year")
gloeo_Midge_annual_precip2 <- full_join(gloeo_Midge_surface_mean,gloeo_Midge_annual_precip,by="year")

# Min Aug air temp for each year
min_aug_airtemp <- airtemp_daily %>% #[1:13514,]
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  filter(year > 2004 & year < 2017) %>% 
  filter(month ==8) %>% 
  group_by(year) %>% 
  summarize(aug_min_airtemp = min(airtemp_min_degrees_C))

# Read in air temp data
gloeo_Midge_airtemp <- read_csv("Datasets/Sunapee/Bayes_Covariates_Data/gloeo_Midge_airtemp_precip.csv")

# Filter for season
gloeo_Midge_airtemp_season <- gloeo_Midge_airtemp %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_surface_median <- gloeo_Midge_airtemp_season %>% 
  group_by(year) %>% 
  summarize(median_gloeo = median(totalperL)) %>% 
  mutate(log_median_gloeo =log10(median_gloeo)) 

gloeo_Midge_surface_mean <- gloeo_Midge_airtemp_season %>% 
  group_by(year) %>% 
  summarize(mean_gloeo = mean(totalperL)) %>% 
  mutate(log_mean_gloeo =log10(mean_gloeo))

gloeo_Midge_surface_max <- gloeo_Midge_airtemp_season %>% 
  group_by(year) %>% 
  summarize(max_gloeo = max(totalperL)) %>% 
  mutate(log_max_gloeo =log10(max_gloeo))


# join with min aug air temp
gloeo_Midge_surface <- full_join(gloeo_Midge_surface_median,gloeo_Midge_surface_mean,by="year")
gloeo_Midge_aug_airtemp <- full_join(gloeo_Midge_surface,min_aug_airtemp,by="year")
gloeo_Midge_aug_airtemp2 <- full_join(gloeo_Midge_aug_airtemp,gloeo_Midge_surface_max,by="year")

str(gloeo_Midge_aug_airtemp)

median_gloeo_fit <- lm(log_median_gloeo ~ aug_min_airtemp, data = gloeo_Midge_aug_airtemp)
summary(median_gloeo_fit)

max_gloeo_fit <- lm(log_max_gloeo ~ aug_min_airtemp, data = gloeo_Midge_aug_airtemp2)
summary(max_gloeo_fit)

precip_gloeo_fit <- lm(mean_gloeo ~ sum_precip, data = gloeo_Midge_annual_precip2)
summary(precip_gloeo_fit)

r <- cor(gloeo_Midge_aug_airtemp2$log_max_gloeo, gloeo_Midge_aug_airtemp2$aug_min_airtemp)
r
r <- cor(gloeo_Midge_annual_precip2$log_mean_gloeo, gloeo_Midge_annual_precip2$sum_precip)

# Plot
ggplot(gloeo_Midge_aug_airtemp2, aes(x=aug_min_airtemp,y=log_max_gloeo,color=factor(year)))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = -0.15867, intercept = 1.69837, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = -0.18833, intercept = 0.95874, size=1, linetype =1)+ #1:1 line
  #geom_smooth(method = "lm", se = T)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Annual Median Surface LOG Gloeo Density (colonies/L)", x="Minimum August Air Temp (°C)",color="Year",title = "Midge")+
  #scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  #annotate("text",x=5.5, y=-1, label = "r2 = 0.3 ", size=6, family = "Times")+
  #annotate("text",x=5.5, y=-1.25, label = "r = -0.61 ", size=6, family = "Times")+
  #geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("~/Desktop/Gloeo Plots/MedianGloeo_Min_AugAirTemp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogMedianGloeo_Min_AugAirTemp.jpeg", width = 11, height = 8.5)


# Air Temp Plots ####

#Daily Air Temp
airtemp_daily <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

# Read in air temp data
gloeo_Midge_airtemp <- read_csv("Datasets/Sunapee/Bayes_Covariates_Data/gloeo_Midge_airtemp_precip.csv")

# Filter for season
gloeo_Midge_airtemp_season <- gloeo_Midge_airtemp %>%
 # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_airtemp_season %>% 
  filter(airtemp_max_degrees_C!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ airtemp_max_degrees_C, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$airtemp_max_degrees_C)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$airtemp_max_degrees_C

scale_x <- max(x)-15
scale_y <- min(y)

scale_rx <- max(x)-10
scale_ry <- min(y)-0.5

ggplot(gloeo_Midge_noNA, aes(x=airtemp_max_degrees_C,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Daily Max Air Temp (°C)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_AirTemp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_AirTemp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_AirTemp.jpeg", width = 11, height = 8.5)

# Precip Plots ####

# Read in precip data
gloeo_Midge_precip <- read_csv("Datasets/Sunapee/Bayes_Covariates_Data/gloeo_Midge_airtemp_precip.csv")

# Exploratory Precip Plot
ggplot(gloeo_Midge_precip, aes(x=week,y=ppt_mm,color=log_gloeo))+ #,color=factor(month)))+
  geom_line() +
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  #geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Daily Precipitation (mm)", x="Week",color="Log Gloeo",title = "Midge")+
  scale_color_gradient(limits=c(-2.5,2), low = "blue",high = "red")+ 
  #xlim(19,27)+
  #ylim(0,20)+
  #geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year) #,scales = "free_y"

ggsave("~/Desktop/Gloeo Plots/Precip_Week-freey.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/Precip_Week-fixy.jpeg", width = 11, height = 8.5)

# Filter for season
gloeo_Midge_season <- gloeo_Midge_precip %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(ppt_mm!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ ppt_mm, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$ppt_mm)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$ppt_mm

scale_x <- max(x)-30
scale_y <- min(y)

scale_rx <- max(x)-20
scale_ry <- min(y)-0.5

ggplot(gloeo_Midge_noNA, aes(x=ppt_mm,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Daily Precipitation (mm)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Daily_Precip.jpeg", width = 11, height = 8.5)

# Precip 1 Week Lag ####
gloeo_Midge_precip_1week_lag <- gloeo_Midge_precip %>% 
  mutate(date_1weeklag = date - dweeks(1)) %>% 
  select(date, log_gloeo, date_1weeklag)

precip_daily <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

# Subset for 2005-2016
precip_daily_subset <- precip_daily %>% #[1:13514,]
  mutate(year = year(Date)) %>% 
  #mutate(month = month(Date)) %>% 
  #mutate(dayofmonth = day(Date)) %>%
  rename(date = Date) %>% 
  filter(year > 2004 & year < 2017) 

precip_daily_subset_lag <- precip_daily_subset %>% 
  rename(date_1weeklag = date)

#Join precip with 1 week lag date
join_gloeo_Midge_1week_lag <- left_join(gloeo_Midge_precip_1week_lag, precip_daily_subset_lag,by = "date_1weeklag")

gloeo_Midge_precip_1week_lag <- join_gloeo_Midge_1week_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

# Write Precip 1 week lag
write_csv(gloeo_Midge_precip_1week_lag, "Datasets/Sunapee/SummarizedData/Midge_gloeo_precip_1weekLAG.csv")

#Precip 2 day lag
gloeo_Midge_precip_2day_lag <- gloeo_Midge_precip %>% 
  mutate(date_2daylag = date - ddays(5)) %>% 
  select(date, log_gloeo, totalperL,date_2daylag)

precip_daily <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

# Subset for 2005-2016
precip_daily_subset <- precip_daily %>% #[1:13514,]
  mutate(year = year(Date)) %>% 
  #mutate(month = month(Date)) %>% 
  #mutate(dayofmonth = day(Date)) %>%
  rename(date = Date) %>% 
  filter(year > 2004 & year < 2017) 

precip_daily_subset_lag <- precip_daily_subset %>% 
  rename(date_2daylag = date)

#Join precip with 2 day lag date
join_gloeo_Midge_2day_lag <- left_join(gloeo_Midge_precip_2day_lag, precip_daily_subset_lag,by = "date_2daylag")

gloeo_Midge_precip_2day_lag <- join_gloeo_Midge_2day_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

# Precip LAG plots ####

# Filter for season
gloeo_Midge_season <- gloeo_Midge_precip_2day_lag %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(ppt_mm!="NA")  %>% 
  filter(totalperL!=0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ ppt_mm, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$ppt_mm)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$ppt_mm

scale_x <- max(x)-30
scale_y <- min(y)

scale_rx <- max(x)-20
scale_ry <- min(y)-0.5

ggplot(gloeo_Midge_noNA, aes(x=ppt_mm,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Day Lag Daily Precipitation (mm)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Precip_1dayLag.jpeg", width = 11, height = 8.5)

# Moving sum/avg for Precip ####
library(zoo)

# Read in daily precip data
precip_daily <- read_csv("Datasets/Sunapee/RawData/weather/PRISM_ppttempdata/PRISM_met_1981_2017_midge.csv", skip = 10)

# Subset for 2005-2016
precip_daily_subset <- precip_daily[,1:2] %>% #[1:13514,]
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  #mutate(month = month(Date)) %>% 
  #mutate(dayofmonth = day(Date)) %>%
  rename(date = Date) %>% 
  filter(year > 2004 & year < 2017) %>% 
  filter(month > 3 & month < 11) %>% 
  filter(year==2013)

plot(precip_daily_subset$ppt_mm ~ precip_daily_subset$date)

ma_2013 <- precip_daily_subset[,1:2] %>% #[-c(1:6),]
  mutate(ma_3 = rollmean(ppt_mm, k = 3, fill = NA, align = "right")) %>% 
  mutate(ma_5 = rollmean(ppt_mm, k = 5, fill = NA, align = "right")) %>% 
  mutate(ma_7 = rollmean(ppt_mm, k = 7, fill = NA, align = "right")) %>% 
  mutate(ma_10 = rollmean(ppt_mm, k = 10, fill = NA, align = "right")) %>% 
  mutate(ma_14 = rollmean(ppt_mm, k = 14, fill = NA, align = "right")) 

ma_all <- bind_rows(ma_2005,ma_2006, ma_2007, ma_2008, ma_2009, ma_2010, ma_2011, ma_2012, ma_2013, ma_2014, ma_2015, ma_2016)

plot(onset_wtr_temp ~ datetime, data = midge_watertemp_ma)
lines(ma_7 ~ datetime, data = midge_watertemp_ma, col = "red")
lines(ma_5 ~ datetime, data = midge_watertemp_ma, col = "blue")
lines(ma_3 ~ datetime, data = midge_watertemp_ma, col = "yellow")

write_csv(ma_all,"Datasets/Sunapee/SummarizedData/precip_movingavg.csv")

gloeo_Midge_precip_moving_avg <- left_join(gloeo_Midge_precip,ma_all,by="date")

write_csv(gloeo_Midge_moving_avg,"Datasets/Sunapee/SummarizedData/gloeo_Midge_moving_avg.csv")

sum_2013 <- precip_daily_subset[,1:2] %>% #[-c(1:6),]
  mutate(sum_3 = rollsum(ppt_mm, k = 3, fill = NA, align = "right")) %>% 
  mutate(sum_5 = rollsum(ppt_mm, k = 5, fill = NA, align = "right")) %>% 
  mutate(sum_6 = rollsum(ppt_mm, k = 6, fill = NA, align = "right")) %>% 
  mutate(sum_7 = rollsum(ppt_mm, k = 7, fill = NA, align = "right")) %>% 
  mutate(sum_10 = rollsum(ppt_mm, k = 10, fill = NA, align = "right")) %>% 
  mutate(sum_14 = rollsum(ppt_mm, k = 14, fill = NA, align = "right")) 

sum_all <- bind_rows(sum_2005,sum_2006, sum_2007, sum_2008, sum_2009, sum_2010, sum_2011, sum_2012, sum_2013, sum_2014, sum_2015, sum_2016)

write_csv(sum_all,"Datasets/Sunapee/SummarizedData/precip_movingsum.csv")

gloeo_Midge_precip_moving_sum <- left_join(gloeo_Midge_precip,sum_all,by="date")

write_csv(gloeo_Midge_precip_moving_sum,"Datasets/Sunapee/SummarizedData/gloeo_Midge_precip_moving_sum.csv")

# Precip Moving Avg/Sum plots ####

# Filter for season
gloeo_Midge_season <- gloeo_Midge_precip_moving_sum %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(sum_6!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ sum_6, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$sum_6)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$sum_6

scale_x <- max(x)-100
scale_y <- min(y)

scale_rx <- max(x)-50
scale_ry <- min(y)-0.5

ggplot(gloeo_Midge_noNA, aes(x=sum_6,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="2 Week Cumulative Daily Precipitation (mm)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Precip_1weekCumSum.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Precip_2weekCumSum.jpeg", width = 11, height = 8.5)

# Wind Speed Plots ####

# Raw buoy data
buoy_wind <- read_csv("Datasets/Sunapee/RawData/Sunapee buoy data/met_data/2007-2017_wind_L1.csv", col_types = cols(
  datetime = col_datetime(format = ""),
  location = col_character(),
  WindDir_deg = col_double(),
  WindSp_ms = col_double(),
  AveWindDir_deg = col_double(),
  AveWindSp_ms = col_double(),
  MaxWindSp_ms = col_double(),
  MaxWindDir_deg = col_double()))

buoy_wind_2007_2016 <- buoy_wind %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(month = month(datetime)) %>% 
  filter(year < 2017)

# Wind Speed Summary - Daily No wind Direction filter
gloeo_Midge_wind_sp <- read_csv("Datasets/Sunapee/Bayes_Covariates_Data/gloeo_Midge_windspeed.csv")

# Filter for season
gloeo_Midge_season <- gloeo_Midge_wind_sp %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(MaxWindSp_ms_max!="NA")

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ MaxWindSp_ms_max, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$MaxWindSp_ms_max)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$WindSp_ms_cv

scale_x <- max(x)-0.2
scale_y <- min(y)

scale_rx <- max(x)-0.1
scale_ry <- min(y)+0.5

ggplot(gloeo_Midge_noNA, aes(x=WindSp_ms_cv,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="CV Daily Wind Speed (m/s)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Median_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_CV_InstWindSp.jpeg", width = 11, height = 8.5)

# Filter Instaneous Wind speed for Wind direction coming from >S, SW, W, NW, <N ####

buoy_wind_2007_2016 <- buoy_wind %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(month = month(datetime)) %>% 
  filter(year < 2017)

buoy_wind_2007_2008 <- buoy_wind_2007_2016 %>% 
  filter(year==2007|year==2008) %>% 
  select(date,WindDir_deg,AveWindDir_deg,WindSp_ms, AveWindSp_ms, MaxWindSp_ms) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean, median, min, max, sd), na.rm=T) %>% 
  mutate(WindSp_ms_cv = WindSp_ms_sd/WindSp_ms_mean) %>% 
  mutate(AveWindSp_ms_cv = AveWindSp_ms_sd/AveWindSp_ms_mean) %>% 
  mutate(MaxWindSp_ms_cv = MaxWindSp_ms_sd/MaxWindSp_ms_mean) 

x <- buoy_wind_2007_2008
buoy_wind_2007_2008 <- replace(x,x == Inf|x == -Inf, NA)

windsp_filtered_daily_summary <- buoy_wind_2007_2016 %>% 
  filter(180< WindDir_deg & WindDir_deg < 360) %>%  #2009-April 2015
  select(date,WindDir_deg,WindSp_ms, AveWindSp_ms, MaxWindSp_ms) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean, median, min, max, sd), na.rm=T) %>% 
  mutate(WindSp_ms_cv = WindSp_ms_sd/WindSp_ms_mean) %>% 
  mutate(AveWindSp_ms_cv = AveWindSp_ms_sd/AveWindSp_ms_mean) %>% 
  mutate(MaxWindSp_ms_cv = MaxWindSp_ms_sd/MaxWindSp_ms_mean) 

x <- windsp_filtered_daily_summary
windsp_filtered_daily_summary <- replace(x,x == Inf|x == -Inf, NA)

windsp_filtered_avgdir_daily_summary <- buoy_wind_2007_2016 %>% 
  filter(180< AveWindDir_deg & AveWindDir_deg < 360) %>%  #missing 2011, 2012
  select(date,AveWindDir_deg,WindSp_ms, AveWindSp_ms, MaxWindSp_ms) %>% 
  group_by(date) %>% 
  summarize_all(funs(mean, median, min, max, sd), na.rm=T) %>% 
  mutate(WindSp_ms_cv = WindSp_ms_sd/WindSp_ms_mean) %>% 
  mutate(AveWindSp_ms_cv = AveWindSp_ms_sd/AveWindSp_ms_mean) %>% 
  mutate(MaxWindSp_ms_cv = MaxWindSp_ms_sd/MaxWindSp_ms_mean) 

x <- windsp_filtered_avgdir_daily_summary
windsp_filtered_avgdir_daily_summary <- replace(x,x == Inf|x == -Inf, NA)

maxwindsp_filtered_daily_summary <- buoy_wind_2007_2016 %>% 
  filter(180< MaxWindDir_deg & MaxWindDir_deg < 360) %>%  #missing 2011, 2012
  select(date,MaxWindDir_deg,MaxWindSp_ms) %>% 
  group_by(date) %>% 
  summarize_all(funs(max), na.rm=T) 

write_csv(maxwindsp_filtered_daily_summary,"Datasets/Sunapee/SummarizedData/maxwindsp_filtere_daily_summary.csv")


# Join wind datasets together
windsp_filtered_join <- full_join(windsp_filtered_daily_summary,windsp_filtered_avgdir_daily_summary,by="date")

windsp_filtered_join2 <- full_join(buoy_wind_2007_2008,windsp_filtered_join,by="date")

write_csv(windsp_filtered_join2,"Datasets/Sunapee/SummarizedData/windsp_filtered_join2.csv")

# Read in wind speed data joined with 4 possible combinations
# 1) instantaneous wind direction filtered, instantaneous wind speed data primary, filled with ave dir and ave speed after 4 June 2015
# 2) instantaneous wind direction filtered, avg wind speed data primary
# 3) avg wind direction filtered, instantaneous wind speed data primary
# 4) avg wind direction filtered, avg wind speed data primary

windsp_filtered_all <- read_csv("Datasets/Sunapee/SummarizedData/WindSpeed_Filtered_Compare.csv")

str(windsp_filtered_all)

#Join with gloeo data
gloeo_Midge_instwindsp_filtered <- left_join(gloeo_schmidt_Midge[,1:12],windsp_filtered_daily_summary,by="date")

write_csv(gloeo_Midge_instwindsp_filtered,"Datasets/Sunapee/SummarizedData/gloeo_Midge_instdirwindsp_filtered.csv")

gloeo_Midge_avedirwindsp_filtered <- left_join(gloeo_schmidt_Midge[,1:12],windsp_filtered_avgdir_daily_summary,by="date")

gloeo_Midge_windsp_all <- left_join(gloeo_schmidt_Midge[,1:12],windsp_filtered_all,by="date")

# Filtered Wind Speed Plots ####

# Filter for season
gloeo_Midge_season <- gloeo_Midge_windsp_all %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(MaxWindSp_ms_max_ALL_InstFilterMax!="NA") %>% 
  filter(totalperL!=0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ MaxWindSp_ms_max_ALL_InstFilterMax, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$MaxWindSp_ms_max_ALL_InstFilterMax)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$WindSp_ms_cv_ALL_InstFilterInstMax

scale_x <- max(x)-0.25
scale_y <- min(y)

scale_rx <- max(x)-0.25
scale_ry <- min(y)+0.5

ggplot(gloeo_Midge_noNA, aes(x=WindSp_ms_cv_ALL_InstFilterInstMax,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="CV Daily Wind Speed Filtered (m/s)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Median_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_InstWindSp-Filtered.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_CV_InstWindSp.jpeg", width = 11, height = 8.5)

# 1 week lag wind speed ####
gloeo_Midge_windsp_all_1week_lag <- gloeo_Midge_windsp_all %>% 
  mutate(date_1weeklag = date - dweeks(1)) %>% 
  select(date, log_gloeo,totalperL, date_1weeklag)

# Daily wind speed data
windsp_filtered_all <- read_csv("Datasets/Sunapee/SummarizedData/WindSpeed_Filtered_Compare.csv")

windsp_filtered_all_lag <- windsp_filtered_all %>% 
  rename(date_1weeklag = date)

#Join wind speed with 1 week lag date
join_gloeo_Midge_windsp_1week_lag <- left_join(gloeo_Midge_windsp_all_1week_lag, windsp_filtered_all_lag,by = "date_1weeklag")

gloeo_Midge_windsp_1week_lag <- join_gloeo_Midge_windsp_1week_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

# Write Wind Speed 1 week lag
write_csv(gloeo_Midge_windsp_1week_lag, "Datasets/Sunapee/SummarizedData/gloeo_Midge_windsp_1week_lag.csv")

# 1, 2, 3, day lag wind speed ####
# 1 day
gloeo_Midge_windsp_all_1day_lag <- gloeo_Midge_windsp_all %>% 
  mutate(date_1daylag = date - ddays(1)) %>% 
  select(date, log_gloeo, totalperL, date_1daylag)

# 2 day
gloeo_Midge_windsp_all_2day_lag <- gloeo_Midge_windsp_all %>% 
  mutate(date_2daylag = date - ddays(2)) %>% 
  select(date, log_gloeo, totalperL, date_2daylag)

# 3 day
gloeo_Midge_windsp_all_3day_lag <- gloeo_Midge_windsp_all %>% 
  mutate(date_3daylag = date - ddays(3)) %>% 
  select(date, log_gloeo, totalperL, date_3daylag)

# 5 day
gloeo_Midge_windsp_all_5day_lag <- gloeo_Midge_windsp_all %>% 
  mutate(date_5daylag = date - ddays(5)) %>% 
  select(date, log_gloeo, totalperL, date_5daylag)

# Daily wind speed data
windsp_filtered_all <- read_csv("Datasets/Sunapee/SummarizedData/WindSpeed_Filtered_Compare.csv")

windsp_filtered_all_lag <- windsp_filtered_all %>% 
  rename(date_1daylag = date)

windsp_filtered_all_lag <- windsp_filtered_all %>% 
  rename(date_2daylag = date)

windsp_filtered_all_lag <- windsp_filtered_all %>% 
  rename(date_3daylag = date)

windsp_filtered_all_lag <- windsp_filtered_all %>% 
  rename(date_5daylag = date)

#Join wind speed with 1 day lag date
join_gloeo_Midge_windsp_1day_lag <- left_join(gloeo_Midge_windsp_all_1day_lag, windsp_filtered_all_lag,by = "date_1daylag")

join_gloeo_Midge_windsp_2day_lag <- left_join(gloeo_Midge_windsp_all_2day_lag, windsp_filtered_all_lag,by = "date_2daylag")

join_gloeo_Midge_windsp_3day_lag <- left_join(gloeo_Midge_windsp_all_3day_lag, windsp_filtered_all_lag,by = "date_3daylag")

join_gloeo_Midge_windsp_5day_lag <- left_join(gloeo_Midge_windsp_all_5day_lag, windsp_filtered_all_lag,by = "date_5daylag")

gloeo_Midge_windsp_1day_lag <- join_gloeo_Midge_windsp_1day_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

gloeo_Midge_windsp_2day_lag <- join_gloeo_Midge_windsp_2day_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

gloeo_Midge_windsp_3day_lag <- join_gloeo_Midge_windsp_3day_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

gloeo_Midge_windsp_5day_lag <- join_gloeo_Midge_windsp_5day_lag %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  mutate(week = week(date))

# Wind Speed 1,2,3,5, day & 1 week Lag plots ####

# Filter for season
gloeo_Midge_season <- gloeo_Midge_windsp_1week_lag %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(MaxWindSp_ms_max_ALL_InstFilterMax!="NA") %>% 
  filter(totalperL!= 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ MaxWindSp_ms_max_ALL_InstFilterMax, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$MaxWindSp_ms_max_ALL_InstFilterMax)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$MaxWindSp_ms_max_ALL_InstFilterMax

scale_x <- max(x)-5
scale_y <- min(y)

scale_rx <- max(x)-3
scale_ry <- min(y)+0.5

ggplot(gloeo_Midge_noNA, aes(x=MaxWindSp_ms_max_ALL_InstFilterMax,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Week Lag Max Daily Max Wind Speed Filtered (m/s)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Median_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_InstWindSp-Filtered.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_CV_InstWindSp.jpeg", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_1weekLagMax_MaxWindSp_filtered_nozeros.jpeg", width = 11, height = 8.5)

# Moving sum/avg for Max Wind Speed ####
library(zoo)

# Read in daily wind speed data

windsp_filtered_all <- read_csv("Datasets/Sunapee/SummarizedData/WindSpeed_Filtered_Compare.csv")

# Subset for 2005-2016
windsp_daily_subset <- windsp_filtered_all %>% #[1:13514,]
  mutate(year = year(date)) %>% 
  mutate(month = month(date)) %>% 
  filter(year > 2004 & year < 2017) %>% 
  filter(month > 3 & month < 11) %>% 
  filter(year==2016)

ma_2016 <- windsp_daily_subset[,c(1,26)] %>% #[-c(1:6),]
  mutate(ma_2 = rollmean(MaxWindSp_ms_max_ALL_InstFilterMax, k = 2, fill = NA, align = "right")) %>% 
  mutate(ma_3 = rollmean(MaxWindSp_ms_max_ALL_InstFilterMax, k = 3, fill = NA, align = "right")) %>% 
  mutate(ma_5 = rollmean(MaxWindSp_ms_max_ALL_InstFilterMax, k = 5, fill = NA, align = "right")) %>% 
  mutate(ma_7 = rollmean(MaxWindSp_ms_max_ALL_InstFilterMax, k = 7, fill = NA, align = "right")) %>% 
  mutate(ma_10 = rollmean(MaxWindSp_ms_max_ALL_InstFilterMax, k = 10, fill = NA, align = "right")) %>% 
  mutate(ma_14 = rollmean(MaxWindSp_ms_max_ALL_InstFilterMax, k = 14, fill = NA, align = "right")) 

ma_all <- bind_rows(ma_2007, ma_2008, ma_2009, ma_2010, ma_2011, ma_2012, ma_2013, ma_2014, ma_2015, ma_2016)

plot(onset_wtr_temp ~ datetime, data = midge_watertemp_ma)
lines(ma_7 ~ datetime, data = midge_watertemp_ma, col = "red")
lines(ma_5 ~ datetime, data = midge_watertemp_ma, col = "blue")
lines(ma_3 ~ datetime, data = midge_watertemp_ma, col = "yellow")

write_csv(ma_all,"Datasets/Sunapee/SummarizedData/maxwindp_movingavg.csv")

gloeo_Midge_maxwindsp_moving_avg <- left_join(gloeo_Midge_windsp_all,ma_all,by="date")

write_csv(gloeo_Midge_moving_avg,"Datasets/Sunapee/SummarizedData/gloeo_Midge_moving_avg.csv")


sum_2016 <- windsp_daily_subset[,c(1,26)] %>% #[-c(1:6),]
  mutate(sum_2 = rollsum(MaxWindSp_ms_max_ALL_InstFilterMax, k = 2, fill = NA, align = "right")) %>% 
  mutate(sum_3 = rollsum(MaxWindSp_ms_max_ALL_InstFilterMax, k = 3, fill = NA, align = "right")) %>% 
  mutate(sum_5 = rollsum(MaxWindSp_ms_max_ALL_InstFilterMax, k = 5, fill = NA, align = "right")) %>% 
  mutate(sum_7 = rollsum(MaxWindSp_ms_max_ALL_InstFilterMax, k = 7, fill = NA, align = "right")) %>% 
  mutate(sum_10 = rollsum(MaxWindSp_ms_max_ALL_InstFilterMax, k = 10, fill = NA, align = "right")) %>% 
  mutate(sum_14 = rollsum(MaxWindSp_ms_max_ALL_InstFilterMax, k = 14, fill = NA, align = "right")) 

sum_all <- bind_rows(sum_2007, sum_2008, sum_2009, sum_2010, sum_2011, sum_2012, sum_2013, sum_2014, sum_2015, sum_2016)

sum_all2 <- sum_all %>% 
  rename(sum_2 = ma_2) %>% 
  rename(sum_3 = ma_3) %>% 
  rename(sum_5 = ma_5) %>% 
  rename(sum_7 = ma_7) %>% 
  rename(sum_10 = ma_10) %>% 
  rename(sum_14 = ma_14) 
  
write_csv(sum_all2,"Datasets/Sunapee/SummarizedData/maxwindsp_movingsum.csv")

gloeo_Midge_maxwindsp_moving_sum <- left_join(gloeo_Midge_windsp_all,sum_all2,by="date")

write_csv(gloeo_Midge_maxwindsp_moving_sum,"Datasets/Sunapee/SummarizedData/gloeo_Midge_maxwindsp_moving_sum.csv")


# Moving Sum/Avg Max Wind Speed plots ####

# Filter for season
gloeo_Midge_season <- gloeo_Midge_maxwindsp_moving_avg %>%
  # mutate(week = week(date)) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(ma_14!="NA") %>% 
  filter(totalperL!= 0)

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ ma_14, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$ma_14)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$MaxWindSp_ms_max_ALL_InstFilterMax

scale_x <- max(x)-5
scale_y <- min(y)

scale_rx <- max(x)-3
scale_ry <- min(y)+0.5

ggplot(gloeo_Midge_noNA, aes(x=MaxWindSp_ms_max_ALL_InstFilterMax,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="1 Week Lag Max Daily Max Wind Speed Filtered (m/s)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_Mean_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Median_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_InstWindSp-Filtered.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_InstWindSp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_CV_InstWindSp.jpeg", width = 11, height = 8.5)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_1weekLagMax_MaxWindSp_filtered_nozeros.jpeg", width = 11, height = 8.5)


# Ice off vs. Gloeo ####
ice_off <- read_csv("Datasets/Sunapee/SummarizedData/Sunapee_IceOutDates_31Oct2019.csv")

ice_off_filter <- ice_off %>% 
  rename(year = Year) %>% 
  filter(year > 2004 & year < 2017)

#Join with median gloeo
gloeo_Midge_ice_off <- full_join(gloeo_Midge_surface_median,ice_off_filter,by="year")
gloeo_Midge_ice_off2 <- full_join(gloeo_Midge_surface_mean,gloeo_Midge_ice_off,by="year")

gloeo_Midge_ice_off2 <- gloeo_Midge_ice_off2 %>% 
  filter(year!=2016)


ice_gloeo_fit <- lm(median_gloeo ~ Sunapee_IceOut_DOY, data = gloeo_Midge_ice_off2)
summary(ice_gloeo_fit)

r <- cor(gloeo_Midge_ice_off2$median_gloeo, gloeo_Midge_ice_off2$Sunapee_IceOut_DOY)
r

# Plot
ggplot(gloeo_Midge_ice_off2, aes(x=Sunapee_IceOut_DOY,y=median_gloeo,color=factor(year)))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = -0.15867, intercept = 1.69837, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = -0.18833, intercept = 0.95874, size=1, linetype =1)+ #1:1 line
  #geom_smooth(method = "lm", se = T)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Annual Median Surface Gloeo Density (colonies/L)", x="Day of Ice-off",color="Year",title = "Midge")+
  #scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  #ylim(0,20)+
  #annotate("text",x=5.5, y=-1, label = "r2 = 0.3 ", size=6, family = "Times")+
  #annotate("text",x=5.5, y=-1.25, label = "r = -0.61 ", size=6, family = "Times")+
  #geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  #geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Monthly WQ Samples TP, Chla, SD, Turbidity ####
gloeo_Midge_wq <- read_csv("Datasets/Sunapee/SummarizedData/gloeo_Midge_daily_TP_Chla_SD.csv")

# Filter for season
gloeo_Midge_season <- gloeo_Midge_wq %>%
  mutate(TP_ugL = TP*1000) %>% 
  #mutate(log_airtemp_mean = log10(airtemp_mean_degrees_C)) %>% 
  filter(20 < week, week < 41) 

gloeo_Midge_noNA <- gloeo_Midge_season %>% 
  filter(TURBIDITY!="NA") 

lm_labels <- function(dat) {
  mod <- lm(log_gloeo ~ TURBIDITY, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1],2), round(coef(mod)[2],2))
  r <- cor(dat$log_gloeo, dat$TURBIDITY)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r=r, r2 = r2, stringsAsFactors = F)
}

labels <- ddply(gloeo_Midge_noNA, "year", lm_labels)
labels
mean((labels$r)^2)

y <- gloeo_Midge_noNA$log_gloeo
x <- gloeo_Midge_noNA$TURBIDITY

scale_x <- max(x)-7
scale_y <- min(y)+1

scale_rx <- max(x)-5
scale_ry <- min(y)+0.5

ggplot(gloeo_Midge_noNA, aes(x=TURBIDITY,y=log_gloeo,color=month))+ #,color=factor(month)))+
  geom_point(size=4) +
  #geom_abline(slope = 1.09, intercept = -1.19, size=1, linetype=1)+ #best fit line
  #geom_abline(slope = 1, intercept = 0, size=2, linetype =2)+ #1:1 line
  geom_smooth(method = "lm")+ #, se = FALSE)+
  #geom_smooth(method = "loess")+ #, se = FALSE)+
  labs(y="Log Gloeo (colonies/L)", x="Monthly TP (ug/L)",color="Month",title = "Midge")+
  scale_color_gradient(limits=c(5,10), low = "blue",high = "red")+
  #xlim(19,27)+
  ylim(-2,2)+
  geom_text(x = scale_x, y=scale_y, aes(label = formula), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  geom_text(x = scale_rx, y=scale_ry, aes(label = r2), data = labels, parse = T, hjust = 0,inherit.aes = F)+
  theme_bw(base_family = "Times")+
  theme(plot.title = element_text(size=24),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title = element_text(size=18),
        axis.text = element_text(size=18,color="black"),
        legend.text = element_text(size=18,color="black"),
        strip.text = element_text(size=18,color="black"))+
  theme(panel.border=element_blank(),
        axis.line = element_line(color="black"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(~year)

ggsave("~/Desktop/Gloeo Plots/LogGloeo_TP.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Min_AirTemp.jpeg", width = 11, height = 8.5)
ggsave("~/Desktop/Gloeo Plots/LogGloeo_Max_AirTemp.jpeg", width = 11, height = 8.5)
