#developing prior for observational Gloeo error
library(tidyverse)


midge_1 <- read_csv('Datasets/Sunapee/SummarizedData/All_Sites_Gloeo_light_wtrtemp_airtemp.csv') %>%
  filter(site == "midge",
         !is.na(coloniesperL))
midge_2 <- read_csv('Datasets/Sunapee/SummarizedData/MidgeGloeo0814firstcut.csv') %>%
  rename(year = Year)

midge_all <- left_join(midge_1, midge_2, by = c("year","dayofyr")) %>%
  select(site, date, year, dayofyr, coloniesperL,filbundperL,totalperL,`Mean(GloeoDensity)`) %>%
  mutate(grand_mean_Gloeo = (coloniesperL + `Mean(GloeoDensity)`) /2)

midge_plot <- midge_all %>%
  gather(coloniesperL:`Mean(GloeoDensity)`, key = "Gloeo_data_type",value = "Gloeo") %>%
  filter(Gloeo_data_type %in% c("coloniesperL","Mean(GloeoDensity)"),
         year %in% c(2008:2014))

compare <- ggplot(data = midge_plot, aes(x = date, y = Gloeo, group = Gloeo_data_type, color = Gloeo_data_type))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  theme_bw()
compare

rel_diff <- abs(midge_all$coloniesperL - midge_all$`Mean(GloeoDensity)`)

png("C:/Users/Mary Lofton/Desktop/MEL_Bayes/obs_error.png")
hist(rel_diff, breaks = 108, main = "Abs. value of obs. difference")
legend("topright",legend = "breaks ~= 0.5")
dev.off()

rpd <- (midge_all$coloniesperL - midge_all$`Mean(GloeoDensity)`) / midge_all$grand_mean_Gloeo * 100

png("C:/Users/Mary Lofton/Desktop/MEL_Bayes/obs_error_rpd.png")
hist(rpd, main = "Relative percent difference btwn. Gloeo obs")
dev.off()
