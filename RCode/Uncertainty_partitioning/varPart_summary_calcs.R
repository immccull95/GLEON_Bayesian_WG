##Summary calcs of variance partitioning
#Author: Mary Lofton
# Date: 14OCT19

library(tidyverse)

myfiles <- list.files(path = "./Results/Uncertainty_partitioning/", pattern = "_2015.csv")

dat <- read_csv(paste0("./Results/Uncertainty_partitioning/",myfiles[1]),n_max = 1)
dat[1,]<- NA
dat$Model_name <- NA
dat$Year <- NA
dat$Var_type <- NA


for (i in 1:length(myfiles)){
  
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/",myfiles[i]))
  temp1 <- temp
  
  for (j in 2:nrow(temp)){
    temp1[j,]<-(temp[j,] - temp[j-1,])
  }
  
  temp2 <- temp1*100
  temp2$Model_name <- myfiles[i]
  temp2$Year <- 2015
  
  if(nrow(temp2)==3){
    temp2$Var_type <- c("Initial_conditions","Process","Observation")
  } else if(nrow(temp2)==4) {
    temp2$Var_type <- c("Initial_conditions","Process","Observation","Parameter")
  } else {
    temp2$Var_type <- c("Initial_conditions","Process","Observation","Parameter","Driver")
  }
  
  dat <- rbind(dat,temp2)
}


myfiles <- list.files(path = "./Results/Uncertainty_partitioning/", pattern = "_2016.csv")

for (i in 1:length(myfiles)){
  
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/",myfiles[i]))
  temp1 <- temp
  
  for (j in 2:nrow(temp)){
    temp1[j,]<-(temp[j,] - temp[j-1,])
  }
  
  temp2 <- temp1*100
  temp2$Model_name <- myfiles[i]
  temp2$Year <- 2016
  
  if(nrow(temp2)==3){
    temp2$Var_type <- c("Initial_conditions","Process","Observation")
  } else if(nrow(temp2)==4) {
    temp2$Var_type <- c("Initial_conditions","Process","Observation","Parameter")
  } else {
    temp2$Var_type <- c("Initial_conditions","Process","Observation","Parameter","Driver")
  }
  
  dat <- rbind(dat,temp2)
}

dat <- dat[-1,]

dat0 <- dat %>%
  gather(V1:V20, key = "season_week",value = "percent_var") 

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
  group_by(Var_type) %>%
  summarize(mean_perc_var = mean(percent_var, na.rm = TRUE))

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
  mutate(Var_type = factor(Var_type, levels = rev(c("Initial_conditions","Process","Observation","Parameter","Driver"))))

dat6 <- dat5 %>%
  filter(!Mod_name %in% c("Seasonal_AR_Schmidt_and_Diff","Seasonal_AR_Temp_and_Diff"))

allvar <- ggplot(data = dat6, aes(x = reorder(Mod_name,-mean_perc_var),y = mean_perc_var,color = Var_type,fill = Var_type))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 16, angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 20),
        legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  labs(fill = "Variance type",color = "Variance type")+
  scale_fill_manual(values = c("darkseagreen3","gold","deepskyblue4","coral","gray"))+
  scale_color_manual(values = c("darkseagreen3","gold","deepskyblue4","coral","gray"))+
  xlab("")+
  ylab("Proportion of forecast variance")
ggsave(allvar, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/variance_by_model.png",
       device = "png",height = 10, width = 12, units = "in")

driver <- subset(dat5, Var_type == "Driver") #lower for good models
parameter <- subset(dat5, Var_type == "Parameter") #higher for good models
observation <- subset(dat5, Var_type == "Observation") #lower for good models
process <- subset(dat5, Var_type == "Process") #higher for good models
ic <- subset(dat5,Var_type == "Initial_conditions") #doesn't really change (duh)

#####total variance boxplots
myfiles <- list.files(path = "./Results/Uncertainty_partitioning/", pattern = "_total_var.csv")

dat <- read_csv(paste0("./Results/Uncertainty_partitioning/",myfiles[1]),n_max = 1)
dat[1,]<- NA
dat$Model_name <- NA

for (i in 1:length(myfiles)){
  temp <- read_csv(paste0("./Results/Uncertainty_partitioning/",myfiles[i]))
  
  total_var <- temp[nrow(temp),]
  total_var$Model_name <- myfiles[i]
  dat <- rbind(dat,total_var)
}

dat <- dat[-1,]
dat2 <- dat %>%
  gather(V1:V40, key = "sampling_week",value = "variance")

#isolating model names
check <- str_split_fixed(dat2$Model_name,pattern = "_total_var.csv",n=2)
check1 <- str_split_fixed(check[,1],pattern = "Midge_",n=2)
check1 <- as.data.frame(check)
colnames(check1) <- c("Mod_name","trash")

dat3 <- bind_cols(check1, dat2) %>%
  select(Mod_name, sampling_week, variance)

dat4 <- dat3 %>%
  filter(!Mod_name %in% c("Midge_Seasonal_RandomWalk","Midge_Seasonal_RandomWalk_Obs_error"))

dat5 <- dat3 %>%
  filter(Mod_name %in% c("Midge_Seasonal_RandomWalk","Midge_Seasonal_RandomWalk_Obs_error"))


p1 <- ggplot(data = dat4, aes(x = Mod_name, y = variance))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 20),
        legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  xlab("")+
  ylab("Forecast variance")
p1
ggsave(p1, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/total_var_covars.png",
       device = "png",height = 6, width = 4, units = "in")

p2 <- ggplot(data = dat5, aes(x = Mod_name, y = variance))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 16),axis.title = element_text(size = 20),
        legend.title = element_text(size = 18), legend.text = element_text(size = 16))+
  xlab("")+
  ylab("Forecast variance")
ggsave(p2, filename = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/total_var_random_walks.png",
       device = "png",height = 6, width = 1.3, units = "in")

#summary of variance
dat7 <- dat4 %>%
  group_by(Mod_name)%>%
  summarize(mean_var = mean(variance, na.rm = TRUE))

#example informed and uninformed prior
hist(rgamma(10000,0.001,0.001), breaks = 20, xlab = "Observation error",
     cex.lab = 1.4, cex.axis = 1.4,main = "")
hist(rgamma(10000,15.37,7.84), breaks = 20, xlab = "Observation error",
     cex.lab = 1.4, cex.axis = 1.4,main = "")

