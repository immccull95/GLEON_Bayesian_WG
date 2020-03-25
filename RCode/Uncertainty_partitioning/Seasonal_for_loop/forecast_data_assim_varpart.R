# uncertainty partitioning 


# State-space model - revised by S. LaDeau (11/2017) from the EcoForecast Activity by Michael Dietze, with reference "Ecological Forecasting", chapter 8
# ========================================================
#   
#   The data used for this example are from summer weekly(ish) Gloetrichia echinulata (Gloeo.) sampling at 4 locations in Lake Sunapee, NH. The data are provided by Kathryn Cottingham, and should not be used without permission outside this workshop.

library(tidyverse)
library(lubridate)
library(readxl)
library(rjags)
library(runjags)
library(moments)
library(geosphere)
library(googledrive)
source('RCode/Helper_functions/seasonal_plug_n_play.R')
source('RCode/Helper_functions/forecast_plug_n_play_data_assim.R')
#source('RCode/Helper_functions/get_forecast_data.R')

#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

model_name = 'Seasonal_AR_Mintemp' #pick a model name
model=paste0("RCode/Jags_Models/Seasonal_for_loop/",model_name, '.R') #this is the folder where your models are stored

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 5000 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Data_assim"

#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_22JUL19.csv"))+0.003)
forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
forecast_y <- forecast_y[7:8,]

#for watertemp_min
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_16AUG19.csv"))
Temp <- scale(Temp, center = TRUE, scale = TRUE)
Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv"))
Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)

#for DayLength
DayLength <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/daylength_year_by_week_28JAN20.csv"))

#for max Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv"))

#for Ppt
Ppt <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_summed_precip_10OCT19.csv"))

#for underwater light from HOBOs
Light <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv"))

#for Wnd
Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_wind_perc90_14OCT19.csv"))

#for GDD
GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_28JAN20.csv"))

years <- c(2009:2016)
forecast_years <- c(2015:2016)
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)
site = "Midge"

#for min water temp
week_min = colMeans(Temp_prior, na.rm = TRUE)

#for DayLength
week_avg = colMeans(DayLength, na.rm = TRUE)

#for max Schmidt
week_max = colMeans(Schmidt, na.rm = TRUE)

#for cv Wnd
week_cv = colMeans(Wnd, na.rm = TRUE)

#for combined covariate model
week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
week_avg_S = colMeans(Schmidt, na.rm = TRUE)

#for combined covariate model
week_min_T = colMeans(Temp_prior, na.rm = TRUE)
week_min_S = colMeans(Schmidt, na.rm = TRUE)
week_min_W = colMeans(Wnd, na.rm = TRUE)

#6) Run forecasts with data assimilation

## Forward Simulation
N_weeks <- c(1:40)
observ <- c(forecast_y[1,],forecast_y[2,])
y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)

Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_forecast_05OCT19.csv"))
Temp <- scale(Temp, center = TRUE, scale = TRUE)
observ_Temp <- c(Temp[7,],Temp[8,])

SW <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_forecast_03MAR20.csv"))
SW <- scale(SW, center = TRUE, scale = TRUE)
observ_SW <- c(SW[7,],SW[8,])

GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_forecast_03MAR20.csv"))
GDD <- scale(GDD, center = TRUE, scale = TRUE)
observ_GDD <- c(GDD[7,],GDD[8,])


######## deterministic prediction #######
##Set up forecast

for (i in 1:length(N_weeks)){
  source('RCode/Helper_functions/seasonal_plug_n_play.R')
  
  N_weeks <- c(1:40)
  observ <- c(forecast_y[1,],forecast_y[2,])
  y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
  
  Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_forecast_05OCT19.csv"))
  Temp <- scale(Temp, center = TRUE, scale = TRUE)
  observ_Temp <- c(Temp[7,],Temp[8,])
  
  SW <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_forecast_03MAR20.csv"))
  SW <- scale(SW, center = TRUE, scale = TRUE)
  observ_SW <- c(SW[7,],SW[8,])
  
  GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_forecast_03MAR20.csv"))
  GDD <- scale(GDD, center = TRUE, scale = TRUE)
  observ_GDD <- c(GDD[7,],GDD[8,])
  
  if(i == 1){
    obs_data <- rep(NA,40)
    obs_Temp <- rep(NA,40)
    obs_SW <- rep(NA,40)
    obs_GDD <- rep(NA,40)
  } else{
  obs_data <- rep(NA,40)
  obs_data[1:N_weeks[i-1]] <- observ[1:N_weeks[i-1]]
  
  obs_Temp <- rep(NA,40)
  obs_Temp[1:N_weeks[i-1]] <- observ_Temp[1:N_weeks[i-1]]
  
  obs_SW <- rep(NA,40)
  obs_SW[1:N_weeks[i-1]] <- observ_SW[1:N_weeks[i-1]]
  
  obs_GDD <- rep(NA,40)
  obs_GDD[1:N_weeks[i-1]] <- observ_GDD[1:N_weeks[i-1]]
  
  }
  
  y[7,] <- obs_data[1:20]
  y[8,] <- obs_data[21:40]
  
  Temp[7,] <- obs_Temp[1:20]
  Temp[8,] <- obs_Temp[21:40]
  
  SW[7,] <- obs_SW[1:20]
  SW[8,] <- obs_SW[21:40]
  
  GDD[7,] <- obs_GDD[1:20]
  GDD[8,] <- obs_GDD[21:40]
  
  if(N_weeks[i] %in% c(1:20)){
    y <- y[1:7,]
    Temp <- Temp[1:7,]
    SW <- SW[1:7,]
    GDD <- GDD[1:7,]
    year_no = as.numeric(as.factor(c(2009:2015)))
  } else {
    y <- y[1:8,]
    Temp <- Temp[1:8,]
    SW <- SW[1:8,]
    GDD <- GDD[1:8,]
    year_no = as.numeric(as.factor(c(2009:2016)))
  }
  
  
  jags_plug_ins <- jags_plug_ins(model_name = model_name)
  
  # Now that we've defined the model, the data, and the initialization, we need to send all this info to JAGS, which will return the JAGS model object.
  
  #4) Run model (no edits, unless you want to change # of iterations) -------------------------------------------------------------
  j.model   <- jags.model (file = model,
                           data = jags_plug_ins$data.model,
                           inits = jags_plug_ins$init.model,
                           n.chains = 3)
  
  jags.out <- run.jags(model = model,
                       data = jags_plug_ins$data.model,
                       adapt =  5000, 
                       burnin =  100000, 
                       sample = 5000, 
                       n.chains = 3, 
                       inits=jags_plug_ins$init.model,
                       monitor = jags_plug_ins$variable.namesout.model)
  
  #5) Save and Process Output
  jags.out.mcmc <- as.mcmc.list(jags.out)
  out <- as.matrix(jags.out.mcmc)
  
  # set up IC: sample rows from previous analysis
  prow = sample.int(nrow(out),5000,replace=TRUE)
  week_num = N_weeks[i]
  Nmc = 5000
  colnums = rep(1:20, times = 2)
  
  if(N_weeks[i]==1 | N_weeks[i]==21){ #first week uses IC prior from Holly
    IC = rnorm(Nmc,-5,sqrt(1/100))
  } else if(N_weeks[i] %in% c(2:20)) { #other weeks use last mu from spin-up model calibration
    mycol <- paste0("mu","[7,",colnums[N_weeks[i-1]],"]") 
    IC = out[prow,mycol]
  } else {
    mycol <- paste0("mu","[8,",colnums[N_weeks[i-1]],"]") 
    IC = out[prow,mycol]
  }
  

settings.det <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                 Nmc = 1, #number of Monte Carlo draws
                 IC = mean(IC)) #set initial conditions (will be the same for every model)

#MUST BE EDITED TO REFLECT CORRECT PARAMS FOR MODEL
params.det <- get_params(model_name = model_name, 
                         forecast_type = "det") #choose from det, IC, IC.P, IC.P.O, IC.P.O.R, IC.P.O.Pa, IC.P.O.Pa.D, IC.P.O.R.Pa.D

#Run forecast
det.prediction <- forecast_gloeo(model_name = model_name,
                           params = params.det, #list of params necessary to run that model
                           settings = settings.det) #list of settings including N_out, Nmc, and IC

write.csv(det.prediction,file=file.path(my_directory,paste(site,paste0(model_name,'_det.prediction_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)

######## initial condition uncertainty #######


##Set up forecast
settings.IC <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                    Nmc = Nmc,
                    IC = IC)

params.IC <- get_params(model_name = model_name,
                        forecast_type = "IC")


#Run forecast
forecast.IC <- forecast_gloeo(model_name = model_name,
                              params = params.IC,
                              settings = settings.IC)

write.csv(forecast.IC,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)


###### process uncertainty ######### 

##Set up forecast
settings.IC.P <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                      Nmc = Nmc,
                      IC = IC)

params.IC.P <- get_params(model_name = model_name,
                          forecast_type = "IC.P")

#Run forecast
forecast.IC.P <- forecast_gloeo(model_name = model_name,
                                params = params.IC.P,
                                settings = settings.IC.P)

write.csv(forecast.IC.P,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)



###### observation uncertainty ######### 

##Set up forecast
settings.IC.P.O <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                        Nmc = Nmc,
                        IC = IC)

params.IC.P.O <- get_params(model_name = model_name,
                            forecast_type = "IC.P.O")


#Run forecast
forecast.IC.P.O <- forecast_gloeo(model_name = model_name,
                                  params = params.IC.P.O,
                                  settings = settings.IC.P.O)

write.csv(forecast.IC.P.O,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P.O_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)

if(!model_name %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
###### parameter uncertainty #######

##Set up forecast
settings.IC.P.O.Pa <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                           Nmc = Nmc,
                           IC = IC)

params.IC.P.O.Pa <- get_params(model_name = model_name,
                               forecast_type = "IC.P.O.Pa")


#Run forecast
forecast.IC.P.O.Pa <- forecast_gloeo(model_name = model_name,
                                     params = params.IC.P.O.Pa,
                                     settings = settings.IC.P.O.Pa)

write.csv(forecast.IC.P.O.Pa,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P.O.Pa_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)

}

if(!model_name %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){
  
###### driver uncertainty ########## 

##Set up forecast
settings.IC.P.O.Pa.D <- list(N_out = 5, #length of forecast time points (2 years x 20 weeks)
                             Nmc = Nmc,
                             IC = IC)

params.IC.P.O.Pa.D <- get_params(model_name = model_name,
                                 forecast_type = "IC.P.O.Pa.D")


#Run forecast
forecast.IC.P.O.Pa.D <- forecast_gloeo(model_name = model_name,
                                       params = params.IC.P.O.Pa.D,
                                       settings = settings.IC.P.O.Pa.D)

write.csv(forecast.IC.P.O.Pa.D,file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.IC.P.O.Pa.D_',N_weeks[i],'.csv'),sep = '_')),row.names = FALSE)
}

}



##############CALCULATING TOTAL AND RELATIVE VARIANCE FROM FORECASTS
model_names <- c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error",
                 "Seasonal_AR","Seasonal_AR_Mintemp",
                 "Seasonal_DayLength_Quad","Seasonal_DayLength_Quad_Mintemp")
forecast_week = 4

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

vardat.IC <- vardat.IC[,c(1:16,21:36)]
vardat.IC.P <- vardat.IC.P[,c(1:16,21:36)]
vardat.IC.P.O <- vardat.IC.P.O[,c(1:16,21:36)]
vardat.IC.P.O.Pa <- vardat.IC.P.O.Pa[,c(1:16,21:36)]
vardat.IC.P.O.Pa.D <- vardat.IC.P.O.Pa.D[,c(1:16,21:36)]



### calculation of variances
source('RCode/Helper_functions/forecast_plug_n_play_data_assim.R')

varMat   <- make_varMat(model_name = model_names[j])
rowMeans(varMat, na.rm = TRUE)
write.csv(varMat, file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_names[j],'_total_var.csv'), sep = '_')),row.names = FALSE)

###consider adding code here to make sure the intervals are ordered from smallest to greatest 
#to avoid weird overlapping when plotting due to small decreases in predictions
#with all uncertainties incorporated due to chance
V.pred.rel.2015 <- apply(varMat[,1:16],2,function(x) {x/max(x)})
V.pred.rel.2016 <- apply(varMat[,17:32],2,function(x) {x/max(x)})
write.csv(V.pred.rel.2015,file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_names[j],'_varMat_2015.csv'), sep = '_')),row.names = FALSE)
write.csv(V.pred.rel.2016,file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_names[j],'_varMat_2016.csv'), sep = '_')),row.names = FALSE)


# #plot variances
# dev.off(dev.list()["RStudioGD"])
# plot_varMat(model_name = model_name)
#
# ## write stacked area plot to file
# png(file=file.path(my_directory,paste(site,paste0(model_name,'_var_part.png'), sep = '_')), res=300, width=30, height=10, units='cm')
# plot_varMat(model_name = model_name)
# dev.off()
#
#
# ##looking at percentile of obs in forecast distribution
forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
forecast_y <- exp(forecast_y[7:8,])
forecast_ys <- forecast_y[,-c(17:20)]

obs_quantile <- NULL
if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error")){
for (i in 1:length(forecast_ys)){
percentile1 <- ecdf(exp(vardat.IC.P.O.Pa[,i])) ##be sure to change this as needed - needs to be made into a function!!!
obs_quantile[i] <- percentile1(forecast_ys[i])
}} else if(!model_names[j] %in% c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error","Seasonal_AR")){
  for (i in 1:length(forecast_ys)){
    percentile1 <- ecdf(exp(vardat.IC.P.O.Pa.D[,i])) ##be sure to change this as needed - needs to be made into a function!!!
    obs_quantile[i] <- percentile1(forecast_ys[i])
  }} else{
    for (i in 1:length(forecast_ys)){
      percentile1 <- ecdf(exp(vardat.IC.P.O[,i])) ##be sure to change this as needed - needs to be made into a function!!!
      obs_quantile[i] <- percentile1(forecast_ys[i])
    }}


#should add vertical line at 0.5 to this
png(file=file.path(my_directory,paste(site,paste0(model_names[j],'_obs_decile_4wk.png'), sep = '_')), res=300, width=10, height=10, units='cm')
hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
     cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.1))
dev.off()

png(file=file.path(my_directory,paste(site,paste0(model_names[j],'_obs_quartile_4wk.png'), sep = '_')), res=300, width=10, height=10, units='cm')
hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
     cex.axis = 1.2, cex.lab = 1.2, xlim = c(0,1), breaks = seq(0,1,0.25))
dev.off()
}
# #a perfect forecast
# obs_quantile <- rep(0.5, 40)
# png(file=file.path(my_directory,paste("perfect_forecast.png")), res=300, width=10, height=10, units='cm')
# hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
#      cex.axis = 1.2, cex.lab = 1.2, breaks = seq(0,1,0.1))
# dev.off()
#
# #an extremely good forecast
# obs_quantile <- c(0.2, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, rep(0.5,26), 0.6, 0.6, 0.6, 0.6, 0.7, 0.7, 0.8)
# png(file=file.path(my_directory,paste("excellent_forecast.png")), res=300, width=10, height=10, units='cm')
# hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
#      cex.axis = 1.2, cex.lab = 1.2, breaks = seq(0,1,0.1))
# dev.off()
