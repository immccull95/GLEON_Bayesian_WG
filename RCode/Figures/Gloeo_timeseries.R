#####Figure of Gloeo timeseries

library(tidyverse)

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv") %>%
  filter(site == "midge") %>%
  select(date, totalperL)

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"),
                 legend.key = element_blank(),legend.background = element_blank(),
                 text = element_text(size=16), axis.text.y = element_text(size = 16),
                 axis.text.x = element_text(size = 14))

p1 <- ggplot(data = dat, aes(x = date, y = totalperL))+
  geom_point(colour = "darkgreen")+
  geom_line(size = 1, colour = "darkgreen")+
  facet_wrap(vars(year), nrow = 2, ncol = 4, scales = "free_x")+
  ylab("Total Gloeo. colonies per liter")+
  theme(strip.background.x = element_blank())+
  xlab("")+
  mytheme
p1  

ggsave(p1, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Fig3.tif", 
       device = "tiff", height = 6, width = 10, units = "in")

mean(dat$totalperL, na.rm = TRUE)
sd(dat$totalperL, na.rm = TRUE)

max(subset(dat$totalperL, dat$year == 2010))
max(subset(dat$totalperL, dat$year == 2013))

mean(subset(dat$totalperL, dat$year == 2015 | dat$year == 2016), na.rm = TRUE)
sd(subset(dat$totalperL, dat$year == 2015 | dat$year == 2016), na.rm = TRUE)

dat1 <- dat %>%
  group_by(year) %>%
  summarize(max_Gloeo = max(totalperL, na.rm = TRUE))


