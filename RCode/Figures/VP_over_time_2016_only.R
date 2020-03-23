##Final figure of obs in hindcast distributions
library(tidyverse)
library(lubridate)

##############CALCULATING TOTAL AND RELATIVE VARIANCE FROM FORECASTS
model_names <- c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error",
                 "Seasonal_AR","Seasonal_AR_MinSchmidt_Diff",
                 "Seasonal_SWradiation_Quad","Seasonal_AR_Minwind_MinSchmidt_Diff",
                 "Seasonal_AR_SWradiation_MinSchmidt_Diff")
forecast_week = 1

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv") %>%
  filter(site == "midge")

forecast_dat <- dat %>% filter(year(date) %in% c(2016))
forecast_times <- as.Date(as.character(forecast_dat$date))


for (j in 1:length(model_names)){
  my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Data_assim"
  site = "Midge"
  N_weeks <- c(1:20)
  
  det.prediction <- matrix(NA, 1, 20)
  det.prediction <- data.frame(det.prediction)
  
  vardat.IC <- matrix(NA, 5000, 20)
  vardat.IC <- data.frame(vardat.IC)
  
  
  vardat.IC.P <- matrix(NA, 5000, 20)
  vardat.IC.P <- data.frame(vardat.IC.P)
  
  
  vardat.IC.P.O <- matrix(NA, 5000, 20)
  vardat.IC.P.O <- data.frame(vardat.IC.P.O)
  
  vardat.IC.P.O.Pa <- matrix(NA, 5000, 20)
  vardat.IC.P.O.Pa <- data.frame(vardat.IC.P.O.Pa)
  
  
  vardat.IC.P.O.Pa.D <- matrix(NA, 5000, 20)
  vardat.IC.P.O.Pa.D <- data.frame(vardat.IC.P.O.Pa.D)
  
  
  for (i in 1:20){
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_det.prediction_',N_weeks[i],'.csv'),sep = '_')))
    det.prediction[,N_weeks[i]] <- dat[,forecast_week]
    
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
  
  vardat.IC <- vardat.IC[,c(1:20)]
  forecast.ci.IC <- apply(vardat.IC,2,quantile,c(0.005,0.5,0.995), na.rm=TRUE)
  vardat.IC.P <- vardat.IC.P[,c(1:20)]
  forecast.ci.IC.P <- apply(vardat.IC.P,2,quantile,c(0.005,0.5,0.995), na.rm=TRUE)
  vardat.IC.P.O <- vardat.IC.P.O[,c(1:20)]
  forecast.ci.IC.P.O <- apply(vardat.IC.P.O,2,quantile,c(0.005,0.5,0.995), na.rm=TRUE)
  vardat.IC.P.O.Pa <- vardat.IC.P.O.Pa[,c(1:20)]
  forecast.ci.IC.P.O.Pa <- apply(vardat.IC.P.O.Pa,2,quantile,c(0.005,0.5,0.995), na.rm=TRUE)
  vardat.IC.P.O.Pa.D <- vardat.IC.P.O.Pa.D[,c(1:20)]
  forecast.ci.IC.P.O.Pa.D <- apply(vardat.IC.P.O.Pa.D,2,quantile,c(0.005,0.5,0.995), na.rm=TRUE)
  
  # loading in observations
  forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
  forecast_y <- forecast_y[8,]
  forecast_ys <- forecast_y

  #set up ciEnvelope function
  ciEnvelope <- function(x,ylo,yhi,...){
    polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                        ylo[1])), border = NA,...) 
  }
  
  
  if(model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
   #ic + p + o
   lims <- c(min(forecast.ci.IC.P.O[1,c(1:20)])-0.2, max(forecast.ci.IC.P.O[3,c(1:20)])+0.2)
   tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[j],"_ci_log_ICPO_2016.tif")),
       width = 8, height = 6, units = "in", res = 300)
   par(mgp = c(2.3,1,0))
   plot(forecast_times[1:20],forecast_ys[1:20],type = "n",
        xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
        ylim = c(-12,10), main = "1 week ahead hindcasts - 2016")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
   lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
   points(forecast_times[1:20],forecast_ys[1:20],pch = 16)
   dev.off()
   
  } else if(model_names[j] == "Seasonal_AR"){
   
   #ic + p + o + pa
   lims <- c(min(forecast.ci.IC.P.O.Pa[1,c(1:20)])-0.2, max(forecast.ci.IC.P.O.Pa[3,c(1:20)])+0.2)
   tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[j],"_ci_log_ICPOPa_2016.tif")),
       width = 8, height = 6, units = "in", res = 300)
   par(mgp = c(2.3,1,0))
   plot(forecast_times[1:20],forecast_ys[1:20],type = "n",
        xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
        ylim = c(-12,10), main = "1 week ahead hindcasts - 2016")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa[1,c(1:20)],forecast.ci.IC.P.O.Pa[3,c(1:20)],col="gold")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
   lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
   points(forecast_times[1:20],forecast_ys[1:20],pch = 16)
   dev.off()
   
  } else {
   #ic + p + o + pa + d
   lims <- c(min(forecast.ci.IC.P.O.Pa.D[1,c(1:20)])-0.2, max(forecast.ci.IC.P.O.Pa.D[3,c(1:20)])+0.2)
   tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[j],"_ci_log_ICPOPaD_2016.tif")),
        width = 8, height = 6, units = "in", res = 300)
   par(mgp = c(2.3,1,0))
   plot(forecast_times[1:20],forecast_ys[1:20],type = "n",
        xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
        ylim = c(-12,10), main = "1 week ahead hindcasts - 2016")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa.D[1,c(1:20)],forecast.ci.IC.P.O.Pa.D[3,c(1:20)],col="darkseagreen3")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa[1,c(1:20)],forecast.ci.IC.P.O.Pa[3,c(1:20)],col="gold")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
   ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
   lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
   points(forecast_times[1:20],forecast_ys[1:20],pch = 16)
   dev.off()
  }
   
  
}


forecast_week = 4

forecast_times <- forecast_times[c(4:20)]

for (j in 1:length(model_names)){
  my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Data_assim"
  site = "Midge"
  N_weeks <- c(1:20)
  
  det.prediction <- matrix(NA, 1, 20)
  det.prediction <- data.frame(det.prediction)
  
  vardat.IC <- matrix(NA, 5000, 20)
  vardat.IC <- data.frame(vardat.IC)
  
  
  vardat.IC.P <- matrix(NA, 5000, 20)
  vardat.IC.P <- data.frame(vardat.IC.P)
  
  
  vardat.IC.P.O <- matrix(NA, 5000, 20)
  vardat.IC.P.O <- data.frame(vardat.IC.P.O)
  
  vardat.IC.P.O.Pa <- matrix(NA, 5000, 20)
  vardat.IC.P.O.Pa <- data.frame(vardat.IC.P.O.Pa)
  
  
  vardat.IC.P.O.Pa.D <- matrix(NA, 5000, 20)
  vardat.IC.P.O.Pa.D <- data.frame(vardat.IC.P.O.Pa.D)
  
  
  for (i in 1:20){
    
    dat <- read_csv(file=file.path(my_directory,paste(site,paste0(model_names[j],'_det.prediction_',N_weeks[i],'.csv'),sep = '_')))
    det.prediction[,N_weeks[i]] <- dat[,forecast_week]
    
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
  
  det.prediction <- det.prediction[c(1:17)]
  vardat.IC <- vardat.IC[,c(1:17)]
  forecast.ci.IC <- apply(vardat.IC,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  vardat.IC.P <- vardat.IC.P[,c(1:17)]
  forecast.ci.IC.P <- apply(vardat.IC.P,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  vardat.IC.P.O <- vardat.IC.P.O[,c(1:17)]
  forecast.ci.IC.P.O <- apply(vardat.IC.P.O,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  vardat.IC.P.O.Pa <- vardat.IC.P.O.Pa[,c(1:17)]
  forecast.ci.IC.P.O.Pa <- apply(vardat.IC.P.O.Pa,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  vardat.IC.P.O.Pa.D <- vardat.IC.P.O.Pa.D[,c(1:17)]
  forecast.ci.IC.P.O.Pa.D <- apply(vardat.IC.P.O.Pa.D,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
  
  # ##looking at percentile of obs in forecast distribution
  forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
  forecast_y <- forecast_y[8,]
  forecast_ys <- forecast_y[-c(1:3)]

  
  #set up ciEnvelope function
  ciEnvelope <- function(x,ylo,yhi,...){
    polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                        ylo[1])), border = NA,...) 
  }
  
  
  if(model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
    #ic + p + o
    lims <- c(min(forecast.ci.IC.P.O[1,c(1:17)])-0.2, max(forecast.ci.IC.P.O[3,c(1:17)])+0.2)
    tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[j],"_ci_log_ICPO_2016_4wk.tif")),
        width = 8, height = 6, units = "in", res = 300)
    par(mgp = c(2.3,1,0))
    plot(forecast_times[1:17],forecast_ys[1:17],type = "n",
         xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
         ylim = c(-12,10), main = "4 week ahead hindcasts - 2016")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P.O[1,c(1:17)],forecast.ci.IC.P.O[3,c(1:17)],col="deepskyblue4")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P[1,c(1:17)],forecast.ci.IC.P[3,c(1:17)],col="coral")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC[1,c(1:17)],forecast.ci.IC[3,c(1:17)],col="gray")
    lines(forecast_times[1:17],det.prediction[1:17], lwd = 2)
    points(forecast_times[1:17],forecast_ys[1:17],pch = 16)
    dev.off()
    
  } else if(model_names[j] == "Seasonal_AR"){
    
    #ic + p + o + pa
    lims <- c(min(forecast.ci.IC.P.O.Pa[1,c(1:17)])-0.2, max(forecast.ci.IC.P.O.Pa[3,c(1:17)])+0.2)
    tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[j],"_ci_log_ICPOPa_2016_4wk.tif")),
        width = 8, height = 6, units = "in", res = 300)
    par(mgp = c(2.3,1,0))
    plot(forecast_times[1:17],forecast_ys[1:17],type = "n",
         xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
         ylim = c(-12,10), main = "4 week ahead hindcasts - 2016")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P.O.Pa[1,c(1:17)],forecast.ci.IC.P.O.Pa[3,c(1:17)],col="gold")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P.O[1,c(1:17)],forecast.ci.IC.P.O[3,c(1:17)],col="deepskyblue4")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P[1,c(1:17)],forecast.ci.IC.P[3,c(1:17)],col="coral")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC[1,c(1:17)],forecast.ci.IC[3,c(1:17)],col="gray")
    lines(forecast_times[1:17],det.prediction[1:17], lwd = 2)
    points(forecast_times[1:17],forecast_ys[1:17],pch = 16)
    dev.off()
    
  } else {
    #ic + p + o + pa + d
    lims <- c(min(forecast.ci.IC.P.O.Pa.D[1,c(1:17)])-0.2, max(forecast.ci.IC.P.O.Pa.D[3,c(1:17)])+0.2)
    tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[j],"_ci_log_ICPOPaD_2016_4wk.tif")),
        width = 8, height = 6, units = "in", res = 300)
    par(mgp = c(2.3,1,0))
    plot(forecast_times[1:17],forecast_ys[1:17],type = "n",
         xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 2, cex.axis = 2, cex.main = 2,
         ylim = c(-12,10), main = "4 week ahead hindcasts - 2016")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P.O.Pa.D[1,c(1:17)],forecast.ci.IC.P.O.Pa.D[3,c(1:17)],col="darkseagreen3")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P.O.Pa[1,c(1:17)],forecast.ci.IC.P.O.Pa[3,c(1:17)],col="gold")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P.O[1,c(1:17)],forecast.ci.IC.P.O[3,c(1:17)],col="deepskyblue4")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC.P[1,c(1:17)],forecast.ci.IC.P[3,c(1:17)],col="coral")
    ciEnvelope(forecast_times[1:17],forecast.ci.IC[1,c(1:17)],forecast.ci.IC[3,c(1:17)],col="gray")
    lines(forecast_times[1:17],det.prediction[1:17], lwd = 2)
    points(forecast_times[1:17],forecast_ys[1:17],pch = 16)
    dev.off()
    
  }
  
}

