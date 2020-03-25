#Timeseries of environmental covariates used in Bayesian models for hindcasting
library(tidyverse)
library(lubridate)

Temp <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_mintemp_forecast.csv")

SW <- read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_SW_forecast_25FEB20.csv")

Schmidt <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_minSchmidt_05MAR20.csv") %>%
  mutate(Schmidt_diff = c(NA,diff(schmidt)))
Schmidt$Schmidt_diff[c(1,21,41,61,81,101,121,141)] <- NA



mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=16), axis.text.y = element_text(size = 16),
                 axis.text.x = element_text(size = 14))

p2 <- ggplot(data = Schmidt, aes(x = date, y = Schmidt_diff))+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")+
  facet_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("Min. Schmidt stability")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
p2  

ggsave(p2, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Supp_fig_minSchmidt.tif", 
       device = "tiff", height = 6, width = 10, units = "in")

mean(Schmidt$Schmidt_diff, na.rm = TRUE)
sd(Schmidt$Schmidt_diff, na.rm = TRUE)
min(Schmidt$Schmidt_diff, na.rm = TRUE)
max(Schmidt$Schmidt_diff, na.rm = TRUE)

week1 <- Temp %>%
  filter(season_week == 1)

range(week1$watertemp_min, na.rm = TRUE)

week20 <- Temp %>%
  filter(season_week == 20)

range(week20$watertemp_min, na.rm = TRUE)

peak <- Temp %>%
  group_by(year) %>%
  filter(watertemp_min == max(watertemp_min, na.rm = TRUE))

mean(peak$watertemp_min, na.rm = TRUE)
sd(peak$watertemp_min, na.rm = TRUE)


#############SW
p3 <- ggplot(data = SW, aes(x = date, y = SW_median_daily))+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")+
  facet_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression(paste("Median daily SW radiation "," (W ",m^-2,")", sep = "")))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
p3

mean(SW$SW_median_daily, na.rm = TRUE)
sd(SW$SW_median_daily, na.rm = TRUE)


early <- SW %>%
  filter(season_week %in% c(1:11))

range(early$SW_median_daily, na.rm = TRUE)

late <- SW %>%
  filter(season_week %in% c(12:20))

range(late$SW_median_daily, na.rm = TRUE)

peak <- SW %>%
  group_by(year) %>%
  filter(SW_median_daily == max(SW_median_daily, na.rm = TRUE))

mean(peak$SW_median_daily, na.rm = TRUE)
sd(peak$SW_median_daily, na.rm = TRUE)


ggsave(p3, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Supp_fig_SW.tif",
       device = "tiff", height = 6, width = 10, units = "in")

############Minwind
#############SW
p4 <- ggplot(data = Minwind, aes(x = date, y = WindSp_ms_min))+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")+
  facet_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab(expression(paste("Minimum daily wind speed  "," (m ",s^-1,")", sep = "")))+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
p4

mean(Minwind$WindSp_ms_min, na.rm = TRUE)
sd(Minwind$WindSp_ms_min, na.rm = TRUE)


range(Minwind$WindSp_ms_min, na.rm = TRUE)

peak <- Minwind %>%
  group_by(year) %>%
  filter(WindSp_ms_min == max(WindSp_ms_min, na.rm = TRUE))

mean(peak$SW_median_daily, na.rm = TRUE)
sd(peak$SW_median_daily, na.rm = TRUE)


ggsave(p4, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Supp_fig_minwind.tif",
       device = "tiff", height = 6, width = 10, units = "in")



