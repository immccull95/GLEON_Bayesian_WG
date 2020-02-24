#Timeseries of environmental covariates used in Bayesian models for hindcasting

Temp <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_mintemp_forecast.csv")

DayLength <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_daylength_forecast.csv")

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=16), axis.text.y = element_text(size = 16),
                 axis.text.x = element_text(size = 14))

p2 <- ggplot(data = Temp, aes(x = date, y = watertemp_min))+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")+
  facet_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("Min. water temp")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
p2  

ggsave(p2, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Supp_fig_mintemp.tif", 
       device = "tiff", height = 6, width = 10, units = "in")

mean(Temp$watertemp_min, na.rm = TRUE)
sd(Temp$watertemp_min, na.rm = TRUE)

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

p3 <- ggplot(data = DayLength, aes(x = date, y = daylength))+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")+
  facet_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("Day length (hrs)")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
p3

week1 <- DayLength %>%
  filter(season_week == 1)

range(week1$daylength, na.rm = TRUE)

week20 <- DayLength %>%
  filter(season_week == 20)

range(week20$daylength, na.rm = TRUE)

peak <- DayLength %>%
  group_by(year) %>%
  filter(daylength == max(daylength, na.rm = TRUE))

mean(peak$daylength, na.rm = TRUE)
sd(peak$daylength, na.rm = TRUE)


# ggsave(p1, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Fig3.tif", 
#        device = "tiff", height = 6, width = 10, units = "in")