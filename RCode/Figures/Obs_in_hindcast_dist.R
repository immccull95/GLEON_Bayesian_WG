##Final figure of obs in hindcast distributions
library(tidyverse)

##############CALCULATING TOTAL AND RELATIVE VARIANCE FROM FORECASTS
model_names <- c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error",
                 "Seasonal_AR","Seasonal_AR_Mintemp",
                 "Seasonal_DayLength_Quad","Seasonal_DayLength_Quad_Mintemp")
mod_abbrevs <- c("a. RW","b. RW_obs","e. AR","f. AR_mintemp","i. Quad_daylength","j. Daylength_mintemp")
forecast_week = 1

tiff(file = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/FigX_1wk.tif", res = 600, width=4, height=6, units='in')
par(mfrow = c(3,2))


for (j in 1:length(model_names)){
  my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Data_assim"
  site = "Midge"
  N_weeks <- c(1:40)
  
  vardat.IC <- matrix(NA, 5000, 40)
  vardat.IC <- data.frame(vardat.IC)
  
  
  vardat.IC.P <- matrix(NA, 5000, 40)
  vardat.IC.P <- data.frame(vardat.IC.P)
  
  
  vardat.IC.P.O <- matrix(NA, 5000, 40)
  vardat.IC.P.O <- data.frame(vardat.IC.P.O)
  
  vardat.IC.P.O.Pa <- matrix(NA, 5000, 40)
  vardat.IC.P.O.Pa <- data.frame(vardat.IC.P.O.Pa)
  
  
  vardat.IC.P.O.Pa.D <- matrix(NA, 5000, 40)
  vardat.IC.P.O.Pa.D <- data.frame(vardat.IC.P.O.Pa.D)
  
  
  for (i in 1:40){
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC[,N_weeks[i]] <- dat[,forecast_week]
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC.P[,i] <- dat[,forecast_week]
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC.P.O[,i] <- dat[,forecast_week]
    
    if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
      dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O.Pa_',N_weeks[i],'.csv'),sep = '_')))
      vardat.IC.P.O.Pa[,i] <- dat[,forecast_week]}
    
    if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){
      dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O.Pa.D_',N_weeks[i],'.csv'),sep = '_')))
      vardat.IC.P.O.Pa.D[,i] <- dat[,forecast_week]}
    
  }
  
  vardat.IC <- vardat.IC[,c(1:20,21:40)]
  vardat.IC.P <- vardat.IC.P[,c(1:20,21:40)]
  vardat.IC.P.O <- vardat.IC.P.O[,c(1:20,21:40)]
  vardat.IC.P.O.Pa <- vardat.IC.P.O.Pa[,c(1:20,21:40)]
  vardat.IC.P.O.Pa.D <- vardat.IC.P.O.Pa.D[,c(1:20,21:40)]
  
  # ##looking at percentile of obs in forecast distribution
  forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
  forecast_y <- exp(forecast_y[7:8,])
  forecast_ys <- forecast_y
  
  obs_quantile <- NULL
  obs_diff <- NULL
  perc_ys <- c(forecast_ys[1,],forecast_ys[2,])

  if(model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
    pi <- apply(exp(vardat.IC.P.O),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    for (i in 1:length(perc_ys)){
      percentile1 <- ecdf(exp(vardat.IC.P.O[,i])) ##be sure to change this as needed - needs to be made into a function!!!
      obs_quantile[i] <- percentile1(perc_ys[i])
      obs_diff[i]=pi[2,i]-perc_ys[i]
    }} else if(model_names[j] == "Seasonal_AR"){
      pi <- apply(exp(vardat.IC.P.O.Pa),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
      for (i in 1:length(perc_ys)){
        percentile1 <- ecdf(exp(vardat.IC.P.O.Pa[,i])) ##be sure to change this as needed - needs to be made into a function!!!
        obs_quantile[i] <- percentile1(perc_ys[i])
        obs_diff[i]=pi[2,i]-perc_ys[i]
      }} else{
        pi <- apply(exp(vardat.IC.P.O.Pa.D),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
        for (i in 1:length(perc_ys)){
          percentile1 <- ecdf(exp(vardat.IC.P.O.Pa.D[,i])) ##be sure to change this as needed - needs to be made into a function!!!
          obs_quantile[i] <- percentile1(perc_ys[i])
          obs_diff[i]=pi[2,i]-perc_ys[i]
        }}
  
  sink(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Hindcast_summary/",paste0(model_names[j],'_obs_pred_differences.txt')))
  
  #Mean of difference between pred and obs
  obspred_mean=mean(obs_diff, na.rm=TRUE)
  print("Mean of difference between pred including observation error and obs")
  print(obspred_mean)
  
  #Maximum of difference between pred and obs
  obspred_max=max(abs(obs_diff), na.rm=TRUE)
  print("Maximum of difference between pred including observation error and obs")
  print(obspred_max)
  
  #Mean quantile of obs in distribution of pred including observation error
  obs_quantile_mean = mean(obs_quantile, na.rm = TRUE)
  print("Mean quantile of obs in distribution of pred including observation error")
  print(obs_quantile_mean)
  
  #Quantile of 2013 bloom point
  print("Quantile of highest 2015 density in distribution of pred including observation error")
  print(obs_quantile[17])
  
  #percent of time we are predicting negative Gloeo
  print("Percent of time we are predicting negative Gloeo")
  print(length(subset(pi[2,],pi[2,] < 0))/length(pi[2,])*100)
  
  #correlation coefficient of pred vs. obs
  cor.coef <- cor(perc_ys,pi[2,], method = "pearson", use = "complete.obs")
  print("Pearson's correlation coefficient of observations and 50th quantile of predicted")
  print(cor.coef)
  
  #Mean range of 95% predictive interval
  mean_range_pred <- mean(pi[3,]-pi[1,], na.rm = TRUE)
  print("Mean range of 95% confidence interval including observation error")
  print(mean_range_pred)
  sink()
  
  #should add vertical line at 0.5 to this
  hist(obs_quantile,xlab = "Quantile of obs.", 
       cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.1),main = "")
  abline(v = 0.5, lwd = 2)
  abline(v = mean(obs_quantile, na.rm = TRUE),lty = 2, lwd = 2)
  title(mod_abbrevs[j],adj = 0)

  # hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
  #      cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.25))
  # abline(v = 0.5, lwd = 2)
  # abline(v = mean(obs_quantile, na.rm = TRUE),lty = 2, lwd = 2)
}
dev.off()

mod_abbrevs <- c("c. RW","d. RW_obs","g. AR","h. AR_mintemp","k. Quad_daylength","l. Daylength_mintemp")

forecast_week = 4

tiff(file = "C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/FigX_4wk.tif", res = 600, width=4, height=6, units='in')
par(mfrow = c(3,2))

for (j in 1:length(model_names)){
  my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Data_assim"
  site = "Midge"
  N_weeks <- c(1:40)
  
  vardat.IC <- matrix(NA, 5000, 40)
  vardat.IC <- data.frame(vardat.IC)
  
  
  vardat.IC.P <- matrix(NA, 5000, 40)
  vardat.IC.P <- data.frame(vardat.IC.P)
  
  
  vardat.IC.P.O <- matrix(NA, 5000, 40)
  vardat.IC.P.O <- data.frame(vardat.IC.P.O)
  
  vardat.IC.P.O.Pa <- matrix(NA, 5000, 40)
  vardat.IC.P.O.Pa <- data.frame(vardat.IC.P.O.Pa)
  
  
  vardat.IC.P.O.Pa.D <- matrix(NA, 5000, 40)
  vardat.IC.P.O.Pa.D <- data.frame(vardat.IC.P.O.Pa.D)
  
  
  for (i in 1:40){
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC[,N_weeks[i]] <- dat[,forecast_week]
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC.P[,i] <- dat[,forecast_week]
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O_',N_weeks[i],'.csv'),sep = '_')))
    vardat.IC.P.O[,i] <- dat[,forecast_week]
    
    if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
      dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O.Pa_',N_weeks[i],'.csv'),sep = '_')))
      vardat.IC.P.O.Pa[,i] <- dat[,forecast_week]}
    
    if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){
      dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_forecast.IC.P.O.Pa.D_',N_weeks[i],'.csv'),sep = '_')))
      vardat.IC.P.O.Pa.D[,i] <- dat[,forecast_week]}
    
  }
  
  vardat.IC <- vardat.IC[,c(1:17,21:37)]
  vardat.IC.P <- vardat.IC.P[,c(1:17,21:37)]
  vardat.IC.P.O <- vardat.IC.P.O[,c(1:17,21:37)]
  vardat.IC.P.O.Pa <- vardat.IC.P.O.Pa[,c(1:17,21:37)]
  vardat.IC.P.O.Pa.D <- vardat.IC.P.O.Pa.D[,c(1:17,21:37)]
  
  # ##looking at percentile of obs in forecast distribution
  forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
  forecast_y <- exp(forecast_y[7:8,])
  forecast_ys <- forecast_y[,-c(1:3)]
  
  obs_quantile <- NULL
  obs_diff <- NULL
  perc_ys <- c(forecast_ys[1,],forecast_ys[2,])
  
  if(model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
    pi <- apply(exp(vardat.IC.P.O),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
    for (i in 1:length(perc_ys)){
      percentile1 <- ecdf(exp(vardat.IC.P.O[,i])) ##be sure to change this as needed - needs to be made into a function!!!
      obs_quantile[i] <- percentile1(perc_ys[i])
      obs_diff[i]=pi[2,i]-perc_ys[i]
    }} else if(model_names[j] == "Seasonal_AR"){
      pi <- apply(exp(vardat.IC.P.O.Pa),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
      for (i in 1:length(perc_ys)){
        percentile1 <- ecdf(exp(vardat.IC.P.O.Pa[,i])) ##be sure to change this as needed - needs to be made into a function!!!
        obs_quantile[i] <- percentile1(perc_ys[i])
        obs_diff[i]=pi[2,i]-perc_ys[i]
      }} else{
        pi <- apply(exp(vardat.IC.P.O.Pa.D),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
        for (i in 1:length(perc_ys)){
          percentile1 <- ecdf(exp(vardat.IC.P.O.Pa.D[,i])) ##be sure to change this as needed - needs to be made into a function!!!
          obs_quantile[i] <- percentile1(perc_ys[i])
          obs_diff[i]=pi[2,i]-perc_ys[i]
        }}
  
  sink(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Hindcast_summary/",paste0(model_names[j],'_obs_pred_differences_4wk.txt')))
  
  #Mean of difference between pred and obs
  obspred_mean=mean(obs_diff, na.rm=TRUE)
  print("Mean of difference between pred including observation error and obs")
  print(obspred_mean)
  
  #Maximum of difference between pred and obs
  obspred_max=max(abs(obs_diff), na.rm=TRUE)
  print("Maximum of difference between pred including observation error and obs")
  print(obspred_max)
  
  #Mean quantile of obs in distribution of pred including observation error
  obs_quantile_mean = mean(obs_quantile, na.rm = TRUE)
  print("Mean quantile of obs in distribution of pred including observation error")
  print(obs_quantile_mean)
  
  #Quantile of 2013 bloom point
  print("Quantile of highest 2015 density in distribution of pred including observation error")
  print(obs_quantile[14])
  
  #percent of time we are predicting negative Gloeo
  print("Percent of time we are predicting negative Gloeo")
  print(length(subset(pi[2,],pi[2,] < 0))/length(pi[2,])*100)
  
  #correlation coefficient of pred vs. obs
  cor.coef <- cor(perc_ys,pi[2,], method = "pearson", use = "complete.obs")
  print("Pearson's correlation coefficient of observations and 50th quantile of predicted")
  print(cor.coef)
  
  #Mean range of 95% predictive interval
  mean_range_pred <- mean(pi[3,]-pi[1,], na.rm = TRUE)
  print("Mean range of 95% confidence interval including observation error")
  print(mean_range_pred)
  sink()
  
  #should add vertical line at 0.5 to this
  hist(obs_quantile,xlab = "Quantile of obs.", 
       cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.1),main = "")
  abline(v = 0.5, lwd = 2)
  abline(v = mean(obs_quantile, na.rm = TRUE),lty = 2, lwd = 2)
  title(mod_abbrevs[j],adj = 0)
  
  # hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
  #      cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.25))
  # abline(v = 0.5, lwd = 2)
  # abline(v = mean(obs_quantile, na.rm = TRUE),lty = 2, lwd = 2)
}
dev.off()
