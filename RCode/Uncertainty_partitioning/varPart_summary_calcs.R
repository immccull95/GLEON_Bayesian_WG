##Summary calcs of variance partitioning
#Author: Mary Lofton
# Date: 14OCT19

library(tidyverse)
library(cowplot)

################1 week ahead hindcasts
myfiles <- list.files(path = "./Results/Uncertainty_partitioning/w_Data_assim_1wk/", pattern = "_2016.csv")

datum <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_1wk/",myfiles[1]),n_max = 1)
datum[1,]<- NA
datum$Model_name <- NA
datum$Year <- NA
datum$Var_type <- NA

for (i in 1:length(myfiles)){
  
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_1wk/",myfiles[i]))
  temp1 <- temp
  
  for (j in 2:nrow(temp)){
    temp1[j,]<-(temp[j,] - temp[j-1,])
  }
  
  temp2 <- temp1*100
  temp2$Model_name <- myfiles[i]
  temp2$Year <- 2016
  
  if(nrow(temp2)==3){
    temp2$Var_type <- c("Initial conditions","Process","Observation")
  } else if(nrow(temp2)==4) {
    temp2$Var_type <- c("Initial conditions","Process","Observation","Parameter")
  } else {
    temp2$Var_type <- c("Initial conditions","Process","Observation","Parameter","Driver")
  }
  
  datum <- rbind(datum,temp2)
}

datum <- datum[-1,]

dat <- datum 

dat_z <- dat %>%
  filter(Var_type == "Parameter")

dat0 <- dat %>%
  gather(X1:X20, key = "season_week",value = "percent_var") 

#isolating model names
check <- str_split_fixed(dat0$Model_name,pattern = "_2015.csv",n=2)
check1 <- str_split_fixed(check[,1],pattern = "_2016.csv",n=2)
check2 <- str_split_fixed(check1[,1],pattern = "_varMat",n=2)
check3 <- str_split_fixed(check2[,1],pattern = "Midge_",n=2)
check3 <- as.data.frame(check3)
colnames(check3) <- c("trash","Mod_name")

dat1 <- bind_cols(check3, dat0) %>%
  select(Mod_name, Year, season_week, Var_type,percent_var) 

#group by variance type
dat2 <- dat1 %>%
  group_by(Mod_name, Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE),
            min_perc_var = min(percent_var, na.rm = TRUE),
            max_perc_var = max(percent_var, na.rm = TRUE)) %>%
  filter(Var_type == "Driver")

#group by year and variance type
dat3 <- dat1 %>%
  group_by(Year,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE))

#group by model, year, and variance type
dat4 <- dat1 %>%
  group_by(Mod_name,Year,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE))

#group by model and variance type
dat5 <- dat1 %>%
  group_by(Mod_name,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE)) %>%
  mutate(Var_type = factor(Var_type, levels = rev(c("Initial conditions","Process","Observation","Parameter","Driver"))))

proc <- dat5 %>% filter(Var_type == "Process")
obs <- dat5 %>% filter(Var_type == "Observation")
init <- dat5 %>% filter(Var_type == "Initial conditions")
parm <- dat5 %>% filter(Var_type == "Parameter")
driv <- dat5 %>% filter(Var_type == "Driver")

dat6 <- dat5 %>%
  ungroup() %>%
  mutate(Mod_name = ifelse(Mod_name == "Seasonal_AR","AR",
                           ifelse(Mod_name == "Seasonal_AR_MinSchmidt_Diff","AR_minSchmidtdiff",
                                  ifelse(Mod_name == "Seasonal_SWradiation_Quad","Quad_SWradiation",
                                         ifelse(Mod_name == "Seasonal_AR_Minwind_MinSchmidt_Diff","MinSchmidtdiff_minwind",
                                                ifelse(Mod_name == "Seasonal_RandomWalk","RW",
                                                       ifelse(Mod_name == "Seasonal_RandomWalk_Obs_error","RW_obs",
                                                              ifelse(Mod_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff","MinSchmidtdiff_SWradiation",Mod_name)))))))) %>%
  mutate(Mod_name = factor(Mod_name, levels = c("RW","RW_obs","AR","AR_minSchmidtdiff","Quad_SWradiation","MinSchmidtdiff_minwind","MinSchmidtdiff_SWradiation")))

allvar <- ggplot(data = dat6, aes(x = Mod_name,y = mean_perc_var,color = Var_type,fill = Var_type))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 14), legend.text = element_text(size = 13),
        legend.position = "top")+
  labs(fill = "Variance type:",color = "Variance type:")+
  scale_fill_manual(values = c("darkseagreen3","gold","deepskyblue4","coral","gray"))+
  scale_color_manual(values = c("darkseagreen3","gold","deepskyblue4","coral","gray"))+
  xlab("")+
  ylab("Proportion of forecast variance")
allvar
#ggsave(allvar, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/variance_by_model_forecast_w_data_assim.png",
       #device = "png",height = 7, width = 9, units = "in")

driver <- subset(dat6, Var_type == "Driver") #lower for good models
max(driver$mean_perc_var, na.rm = TRUE)
parameter <- subset(dat6, Var_type == "Parameter") #higher for good models
min(parameter$mean_perc_var, na.rm = TRUE)
observation <- subset(dat6, Var_type == "Observation") #lower for good models
mean(observation$mean_perc_var, na.rm = TRUE)
process <- subset(dat6, Var_type == "Process") #higher for good models
mean(process$mean_perc_var, na.rm = TRUE)
ic <- subset(dat6,Var_type == "Initial conditions") #doesn't really change (duh)
mean(ic$mean_perc_var, na.rm = TRUE)

#####total variance boxplots
myfiles <- list.files(path = "./Results/Uncertainty_partitioning/w_Data_assim_1wk/", pattern = "_total_var.csv")

dat <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_1wk/",myfiles[1]),n_max = 1)
dat[1,]<- NA
dat$Model_name <- NA

for (i in 1:length(myfiles)){
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_1wk/",myfiles[i]))
  
  total_var <- temp[2,] - temp[1,]
  total_var$Model_name <- myfiles[i]
  dat <- rbind(dat,total_var)
}

dat <- dat[-1,]
dat2 <- dat %>%
  #select(-X1,-X21) %>%
  gather(X1:X20, key = "sampling_week",value = "variance")

#isolating model names
check <- str_split_fixed(dat2$Model_name,pattern = "_total_var.csv",n=2)
check1 <- str_split_fixed(check[,1],pattern = "Midge_",n=2)
check1 <- as.data.frame(check1[,2])
colnames(check1) <- c("Mod_name")

dat3 <- bind_cols(check1, dat2) %>%
  select(Mod_name, sampling_week, variance)

dat4 <- dat3 %>%
  ungroup() %>%
  mutate(Mod_name = ifelse(Mod_name == "Seasonal_AR","AR",
                           ifelse(Mod_name == "Seasonal_AR_MinSchmidt_Diff","AR_minSchmidtdiff",
                                  ifelse(Mod_name == "Seasonal_SWradiation_Quad","Quad_SWradiation",
                                         ifelse(Mod_name == "Seasonal_AR_Minwind_MinSchmidt_Diff","MinSchmidtdiff_minwind",
                                                ifelse(Mod_name == "Seasonal_RandomWalk","RW",
                                                       ifelse(Mod_name == "Seasonal_RandomWalk_Obs_error","RW_obs",
                                                              ifelse(Mod_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff","MinSchmidtdiff_SWradiation",Mod_name)))))))) %>%
  mutate(Mod_name = factor(Mod_name, levels = c("RW","RW_obs","AR","AR_minSchmidtdiff","Quad_SWradiation","MinSchmidtdiff_minwind","MinSchmidtdiff_SWradiation")))

p1 <- ggplot(data = dat4, aes(x = Mod_name, y = exp(variance)))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  xlab("")+
  ylab(expression(paste("Process variance (colonies",~~L^-1, ")"^2)))
  # geom_text(x = 5.5, y = 150000, aes(label = "4 week hindcast"), size = 6)+
  # scale_y_log10(limits = c(0.003,200000))
p1


################4 week ahead hindcasts
myfiles <- list.files(path = "./Results/Uncertainty_partitioning/w_Data_assim_4wk/", pattern = "_2016.csv")

datum <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_4wk/",myfiles[1]),n_max = 1)
datum[1,]<- NA
datum$Model_name <- NA
datum$Year <- NA
datum$Var_type <- NA

for (i in 1:length(myfiles)){
  
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_4wk/",myfiles[i]))
  temp <- temp[do.call(order, temp),]
  temp1 <- temp
  
  for (j in 2:nrow(temp)){
    temp1[j,]<-(temp[j,] - temp[j-1,])
  }
  
  temp2 <- temp1*100
  temp2 <- as_tibble(temp2)
  temp2$Model_name <- myfiles[i]
  temp2$Year <- 2016
  
  if(nrow(temp2)==3){
    temp2$Var_type <- c("Initial conditions","Process","Observation")
  } else if(nrow(temp2)==4) {
    temp2$Var_type <- c("Initial conditions","Process","Observation","Parameter")
  } else {
    temp2$Var_type <- c("Initial conditions","Process","Observation","Parameter","Driver")
  }
  
  datum <- rbind(datum,temp2)
}

datum <- datum[-1,]



dat <- datum

dat0 <- dat %>%
  gather(X1:X17, key = "season_week",value = "percent_var") 

#isolating model names
check <- str_split_fixed(dat0$Model_name,pattern = "_2015.csv",n=2)
check1 <- str_split_fixed(check[,1],pattern = "_2016.csv",n=2)
check2 <- str_split_fixed(check1[,1],pattern = "_varMat",n=2)
check3 <- str_split_fixed(check2[,1],pattern = "Midge_",n=2)
check3 <- as.data.frame(check3)
colnames(check3) <- c("trash","Mod_name")

dat1 <- bind_cols(check3, dat0) %>%
  select(Mod_name, Year, season_week, Var_type,percent_var)

proc <- dat %>% filter(Var_type == "Process")
obs <- dat %>% filter(Var_type == "Observation")
init <- dat %>% filter(Var_type == "Initial conditions")
parm <- dat %>% filter(Var_type == "Parameter")
driv <- dat %>% filter(Var_type == "Driver")

#group by variance type
dat2 <- dat1 %>%
  group_by(Mod_name,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE),
            min_perc_var = min(percent_var, na.rm = TRUE),
            max_perc_var = max(percent_var, na.rm = TRUE)) %>%
  filter(Var_type == "Driver")

#group by year and variance type
dat3 <- dat1 %>%
  group_by(Year,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE))

#group by model, year, and variance type
dat4 <- dat1 %>%
  group_by(Mod_name,Year,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE))

#group by model and variance type
dat5 <- dat1 %>%
  group_by(Mod_name,Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE)) %>%
  mutate(Var_type = factor(Var_type, levels = rev(c("Initial conditions","Process","Observation","Parameter","Driver"))))

dat6 <- dat5 %>%
  ungroup() %>%
  mutate(Mod_name = ifelse(Mod_name == "Seasonal_AR","AR",
                           ifelse(Mod_name == "Seasonal_AR_MinSchmidt_Diff","AR_minSchmidtdiff",
                                  ifelse(Mod_name == "Seasonal_SWradiation_Quad","Quad_SWradiation",
                                         ifelse(Mod_name == "Seasonal_AR_Minwind_MinSchmidt_Diff","MinSchmidtdiff_minwind",
                                                ifelse(Mod_name == "Seasonal_RandomWalk","RW",
                                                       ifelse(Mod_name == "Seasonal_RandomWalk_Obs_error","RW_obs",
                                                              ifelse(Mod_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff","MinSchmidtdiff_SWradiation",Mod_name)))))))) %>%
  mutate(Mod_name = factor(Mod_name, levels = c("RW","RW_obs","AR","AR_minSchmidtdiff","Quad_SWradiation","MinSchmidtdiff_minwind","MinSchmidtdiff_SWradiation")))


allvar_4 <- ggplot(data = dat6, aes(x = Mod_name,y = mean_perc_var,color = Var_type,fill = Var_type))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 14), legend.text = element_text(size = 13),
        legend.position = "top")+
  labs(fill = "Variance type:",color = "Variance type:")+
  scale_fill_manual(values = c("darkseagreen3","gold","deepskyblue4","coral","gray"))+
  scale_color_manual(values = c("darkseagreen3","gold","deepskyblue4","coral","gray"))+
  xlab("")+
  ylab("Proportion of forecast variance")
allvar_4
# ggsave(allvar, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/variance_by_model_forecast_w_data_assim.png",
#        device = "png",height = 10, width = 8, units = "in")

driver <- subset(dat6, Var_type == "Driver") #lower for good models
mean(driver$mean_perc_var, na.rm = TRUE)
parameter <- subset(dat6, Var_type == "Parameter") #higher for good models
mean(parameter$mean_perc_var, na.rm = TRUE)
observation <- subset(dat6, Var_type == "Observation") #lower for good models
mean(observation$mean_perc_var, na.rm = TRUE)
process <- subset(dat6, Var_type == "Process") #higher for good models
mean(process$mean_perc_var, na.rm = TRUE)
ic <- subset(dat6,Var_type == "Initial conditions") #doesn't really change (duh)
mean(ic$mean_perc_var, na.rm = TRUE)

#####total variance boxplots
myfiles <- list.files(path = "./Results/Uncertainty_partitioning/w_Data_assim_4wk/", pattern = "_total_var.csv")

dat <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_4wk/",myfiles[1]),n_max = 1)
dat[1,]<- NA
dat$Model_name <- NA

for (i in 1:length(myfiles)){
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/w_Data_assim_4wk/",myfiles[i]))
  
  total_var <- temp[2,] - temp[1,]
  total_var$Model_name <- myfiles[i]
  dat <- rbind(dat,total_var)
}

dat <- dat[-1,]
dat2 <- dat %>%
  #select(-X1,-X21) %>%
  gather(X1:X17, key = "sampling_week",value = "variance")

#isolating model names
check <- str_split_fixed(dat2$Model_name,pattern = "_total_var.csv",n=2)
check1 <- str_split_fixed(check[,1],pattern = "Midge_",n=2)
check1 <- as.data.frame(check1[,2])
colnames(check1) <- c("Mod_name")

dat3 <- bind_cols(check1, dat2) %>%
  select(Mod_name, sampling_week, variance)

dat4 <- dat3 %>%
  ungroup() %>%
  mutate(Mod_name = ifelse(Mod_name == "Seasonal_AR","AR",
                           ifelse(Mod_name == "Seasonal_AR_MinSchmidt_Diff","AR_minSchmidtdiff",
                                  ifelse(Mod_name == "Seasonal_SWradiation_Quad","Quad_SWradiation",
                                         ifelse(Mod_name == "Seasonal_AR_Minwind_MinSchmidt_Diff","MinSchmidtdiff_minwind",
                                                ifelse(Mod_name == "Seasonal_RandomWalk","RW",
                                                       ifelse(Mod_name == "Seasonal_RandomWalk_Obs_error","RW_obs",
                                                              ifelse(Mod_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff","MinSchmidtdiff_SWradiation",Mod_name)))))))) %>%
  mutate(Mod_name = factor(Mod_name, levels = c("RW","RW_obs","AR","AR_minSchmidtdiff","Quad_SWradiation","MinSchmidtdiff_minwind","MinSchmidtdiff_SWradiation")))

p1_4 <- ggplot(data = dat4, aes(x = Mod_name, y = exp(variance)))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 16),
        legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  xlab("")+
  ylab(expression(paste("Process variance (colonies",~~L^-1, ")"^2)))
  #geom_text(x = 5.5, y = 150000, aes(label = "4 week hindcast"), size = 6)+
  #scale_y_log10(limits = c(0.003,200000))
p1_4



final_plot <- plot_grid(allvar, allvar_4, align = "hv", nrow = 1, ncol = 2,
                        labels=c('a','b'), label_size = 26)

ggsave(final_plot, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/Fig$$.png",
       height = 6, width = 12, units = "in", scale = 1.4)

final_plot1 <- plot_grid(p1, p1_4, align = "hv", nrow = 1, ncol = 2,
                        labels=c('a','b'), label_size = 26)

ggsave(final_plot1, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/process_var.png",
       height = 6, width = 12, units = "in", scale = 1.4)

# ggsave(p1, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/total_var_covars_w_data_assim.png",
#        device = "png",height = 6, width = 4, units = "in")
# 
# p2 <- ggplot(data = dat5, aes(x = Mod_name, y = variance))+
#   geom_boxplot()+
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90),
#         axis.text.y = element_text(size = 16),axis.title = element_text(size = 20),
#         legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
#   xlab("")+
#   ylab("Forecast variance")
# ggsave(p2, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/total_var_random_walksw_data_assim.png",
#        device = "png",height = 6, width = 1.3, units = "in")
# 
# #summary of variance
# dat7 <- dat4 %>%
#   group_by(Mod_name)%>%
#   summarize(mean_var = mean(variance, na.rm = TRUE))
# 
# #example informed and uninformed prior
# hist(rgamma(10000,0.001,0.001), breaks = 20, xlab = "Observation error",
#      cex.lab = 1.4, cex.axis = 1.4,main = "")
# hist(rgamma(10000,15.37,7.84), breaks = 20, xlab = "Observation error",
#      cex.lab = 1.4, cex.axis = 1.4,main = "")
# 
