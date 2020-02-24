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
source('RCode/Helper_functions/forecast_plug_n_play.R')

#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

model_name = 'Seasonal_AR_Mintemp_Lag_Wnd90_Lag' #pick a model name
model=paste0("RCode/Jags_Models/Seasonal_for_loop/",model_name, '.R') #this is the folder where your models are stored

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 1500 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis/Uncertainty_partitioning"

#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_22JUL19.csv"))+0.003)
forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
forecast_y <- forecast_y[7:8,]

#for watertemp_mean
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_11AUG19.csv"))
Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_16AUG19.csv"))

#for watertemp_min
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_16AUG19.csv"))
Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv"))

#for airtemp
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_airtemp_22JUL19.csv"))

#for max Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv"))

#for min Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_min_Schmidt_28JAN20.csv"))

#for GDD
GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_28JAN20.csv"))
GDD <- scale(GDD, center = TRUE, scale = TRUE)

#for DayLength
DayLength <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/daylength_year_by_week_28JAN20.csv"))
DayLength <- scale(DayLength, center = TRUE, scale = TRUE)

#for Ppt
Ppt <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_summed_precip_10OCT19.csv"))

#for PAR
Light <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_mean_buoyPAR_12OCT19.csv"))

#for underwater light from HOBOs
Light <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv"))

#for Wnd
Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/midge_wind_perc90_14OCT19.csv"))
Wnd <- scale(Wnd, center = TRUE, scale = TRUE)

#####for multivariate models

#for watertemp_min
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_16AUG19.csv"))
Temp <- scale(Temp, center = TRUE, scale = TRUE)
Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv"))
Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)

#for max Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv"))
Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)

#for underwater light from HOBOs
Light <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv"))
Light <- scale(Light, center = TRUE, scale = TRUE)


years <- c(2009:2014)
forecast_years <- c(2015:2016)
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)
site = "Midge"

#for water temp
week_avg = colMeans(Temp_prior, na.rm = TRUE)

#for min water temp
week_min = colMeans(Temp_prior, na.rm = TRUE)

#for Schmidt
week_avg = colMeans(Schmidt, na.rm = TRUE)

#for max Schmidt
week_max = colMeans(Schmidt, na.rm = TRUE)

#for min Schmidt
week_min = colMeans(Schmidt, na.rm = TRUE)

#for GDD
week_avg = colMeans(GDD, na.rm = TRUE)

#for DayLength
week_avg = colMeans(DayLength, na.rm = TRUE)

#for precipitation
week_avg = colMeans(Ppt, na.rm = TRUE)

#for PAR
week_avg = colMeans(Light, na.rm = TRUE)

#for underwater light
week_avg = colMeans(Light, na.rm = TRUE)
week_avg[c(19,20)]<- week_avg[18]

#for Wnd
week_avg = colMeans(Wnd, na.rm = TRUE)

#for combined covariate model
week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
week_avg_S = colMeans(Schmidt, na.rm = TRUE)



#3) JAGS Plug-Ins -----------------------------------------------------------------------------------------------------
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

#6) CI, PI, Obs PI Calculations

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv") %>%
  filter(site == "midge")

cal_dat <- dat %>% filter(year(date) %in% c(2009:2014))
forecast_dat <- dat %>% filter(year(date) %in% c(2015:2016))

times <- as.Date(as.character(cal_dat$date))
forecast_times <- as.Date(as.character(forecast_dat$date))
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=c(grep("mu\\[1,", colnames(out)),grep("mu\\[2,", colnames(out)),
      grep("mu\\[3,", colnames(out)),grep("mu\\[4,", colnames(out)),
      grep("mu\\[5,", colnames(out)),grep("mu\\[6,", colnames(out)))
mu = out[,mus]
ci <- exp(apply(mu,2,quantile,c(0.025,0.5,0.975)))

## One step ahead prediction intervals

samp <- sample.int(nrow(out),nsamp)
mu = out[samp,mus] 
Temps=c(Temp[1,], Temp[2,], Temp[3,], Temp[4,], Temp[5,], Temp[6,])
Schmidts=c(Schmidt[1,], Schmidt[2,], Schmidt[3,], Schmidt[4,], Schmidt[5,], Schmidt[6,])
Ppts=c(Ppt[1,], Ppt[2,], Ppt[3,], Ppt[4,], Ppt[5,], Ppt[6,])
Lights=c(Light[1,], Light[2,], Light[3,], Light[4,], Light[5,], Light[6,])
Wnds=c(Wnd[1,], Wnd[2,], Wnd[3,], Wnd[4,], Wnd[5,], Wnd[6,])
GDDs=c(GDD[1,], GDD[2,], GDD[3,], GDD[4,], GDD[5,], GDD[6,])
DayLengths=c(DayLength[1,], DayLength[2,], DayLength[3,], DayLength[4,], DayLength[5,], DayLength[6,])

ys = exp(c(y[1,],y[2,],y[3,],y[4,],y[5,],y[6,]))
forecast_ys = exp(c(forecast_y[1,],forecast_y[2,]))


#get one-step-ahead predictions
preds_plug_ins <- preds_plug_ins(model_name) 

pi <- exp(apply(preds_plug_ins$pred.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))
obs_pi <- exp(apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))


#7) CI, PI, Obs PI Plot
forecast_plot(cal_years = c(2009:2014), 
              forecast_years = NA, #either provide a vector or NA
              is.forecast.ci = "n")

## Forward Simulation
prow = sample.int(nrow(out),1000,replace=TRUE)


######## deterministic prediction #######
##Set up forecast
settings.det <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                 Nmc = 1, #number of Monte Carlo draws
                 IC = cbind(-5,-5)) #set initial conditions (will be the same for every model)

#MUST BE EDITED TO REFLECT CORRECT PARAMS FOR MODEL
params.det <- get_params(model_name = model_name, 
                         forecast_type = "det") #choose from det, IC, IC.P, IC.P.O, IC.P.O.R, IC.P.O.Pa, IC.P.O.Pa.D, IC.P.O.R.Pa.D

#Run forecast
det.prediction <- forecast_gloeo(model_name = model_name,
                           params = params.det, #list of params necessary to run that model
                           settings = settings.det) #list of settings including N_out, Nmc, and IC

## Plot
forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "n") #choose from "y" or "n"

######## initial condition uncertainty #######

# set up IC: sample rows from previous analysis
Nmc = 1000
IC = cbind(rnorm(Nmc,-5,sqrt(1/100)),rnorm(Nmc,-5,sqrt(1/100)))

##Set up forecast
settings.IC <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                     Nmc = Nmc,
                     IC = IC)

params.IC <- get_params(model_name = model_name,
                        forecast_type = "IC")


#Run forecast
forecast.IC <- forecast_gloeo(model_name = model_name,
                                 params = params.IC,
                                 settings = settings.IC)

## Plot
forecast.ci.IC = apply(exp(forecast.IC), 2, quantile, c(0.025,0.5,0.975))
#log
#forecast.ci.IC = apply(forecast.IC, 2, quantile, c(0.025,0.5,0.975))


forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y",
              forecast.ci = forecast.ci.IC) #choose from "y" or "n"

#just a couple of checks to make sure this is behaving as expected
dev.off(dev.list()["RStudioGD"])
hist(exp(IC[,1]))
hist(exp(forecast.IC[,40]))


###### process uncertainty ######### 

##Set up forecast
settings.IC.P <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                    Nmc = Nmc,
                    IC = IC)

params.IC.P <- get_params(model_name = model_name,
                          forecast_type = "IC.P")

#Run forecast
forecast.IC.P <- forecast_gloeo(model_name = model_name,
                              params = params.IC.P,
                              settings = settings.IC.P)

## Plot
forecast.ci.IC.P = apply(exp(forecast.IC.P), 2, quantile, c(0.025,0.5,0.975))
#log
#forecast.ci.IC.P = apply(forecast.IC.P, 2, quantile, c(0.025,0.5,0.975))

forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y",
              forecast.ci = forecast.ci.IC.P) #choose from "y" or "n"

#just a couple of checks to make sure this is behaving as expected
hist(exp(forecast.IC.P[,10]))


###### observation uncertainty ######### 

##Set up forecast
settings.IC.P.O <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                      Nmc = Nmc,
                      IC = IC)

params.IC.P.O <- get_params(model_name = model_name,
                            forecast_type = "IC.P.O")


#Run forecast
forecast.IC.P.O <- forecast_gloeo(model_name = model_name,
                                params = params.IC.P.O,
                                settings = settings.IC.P.O)

## Plot
forecast.ci.IC.P.O = apply(exp(forecast.IC.P.O), 2, quantile, c(0.025,0.5,0.975))
#log
#forecast.ci.IC.P.O = apply(forecast.IC.P.O, 2, quantile, c(0.025,0.5,0.975))


tiff(file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.png'), sep = '_')), res=300, width=40, height=40, units='cm')
forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y", #choose from "y" or "n"
              forecast.ci = forecast.ci.IC.P.O)
dev.off()

###### random effect uncertainty #######
#MOST MODELS DO NOT HAVE THIS!!

##Set up forecast
settings.IC.P.O.R <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                        Nmc = Nmc,
                        IC = IC)

params.IC.P.O.R <- get_params(model_name = model_name,
                            forecast_type = "IC.P.O.R")


#Run forecast
forecast.IC.P.O.R <- forecast_gloeo(model_name = model_name,
                                  params = params.IC.P.O.R,
                                  settings = settings.IC.P.O.R)

## Plot
forecast.ci.IC.P.O.R = apply(exp(forecast.IC.P.O.R), 2, quantile, c(0.025,0.5,0.975))

tiff(file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.png'), sep = '_')), res=300, width=15, height=40, units='cm')
forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y", #choose from "y" or "n"
              forecast.ci = forecast.ci.IC.P.O.R)
dev.off()


###### parameter uncertainty #######

##Set up forecast
settings.IC.P.O.Pa <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                          Nmc = Nmc,
                          IC = IC)

params.IC.P.O.Pa <- get_params(model_name = model_name,
                              forecast_type = "IC.P.O.Pa")


#Run forecast
forecast.IC.P.O.Pa <- forecast_gloeo(model_name = model_name,
                                    params = params.IC.P.O.Pa,
                                    settings = settings.IC.P.O.Pa)

## Plot
forecast.ci.IC.P.O.Pa = apply(exp(forecast.IC.P.O.Pa), 2, quantile, c(0.025,0.5,0.975))
#log
#forecast.ci.IC.P.O.Pa = apply(forecast.IC.P.O.Pa, 2, quantile, c(0.025,0.5,0.975))


tiff(file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.png'), sep = '_')), res=300, width=40, height=40, units='cm')
forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y", #choose from "y" or "n"
              forecast.ci = forecast.ci.IC.P.O.Pa)
dev.off()

###### driver uncertainty ########## 

##Set up forecast
settings.IC.P.O.Pa.D <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                           Nmc = Nmc,
                           IC = IC)

params.IC.P.O.Pa.D <- get_params(model_name = model_name,
                               forecast_type = "IC.P.O.Pa.D")


#Run forecast
forecast.IC.P.O.Pa.D <- forecast_gloeo(model_name = model_name,
                                     params = params.IC.P.O.Pa.D,
                                     settings = settings.IC.P.O.Pa.D)

## Plot
forecast.ci.IC.P.O.Pa.D = apply(exp(forecast.IC.P.O.Pa.D), 2, quantile, c(0.025,0.5,0.975))
#log
#forecast.ci.IC.P.O.Pa.D = apply(forecast.IC.P.O.Pa.D, 2, quantile, c(0.025,0.5,0.975))


tiff(file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.png'), sep = '_')), res=300, width=40, height=40, units='cm')
forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y", #choose from "y" or "n"
              forecast.ci = forecast.ci.IC.P.O.Pa.D)
dev.off()


### calculation of variances
varMat   <- make_varMat(model_name = model_name)
varMat1 <- apply(varMat,2,sort)
write.csv(varMat1, file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_name,'_total_var.csv'), sep = '_')),row.names = FALSE)

###consider adding code here to make sure the intervals are ordered from smallest to greatest 
#to avoid weird overlapping when plotting due to small decreases in predictions
#with all uncertainties incorporated due to chance
V.pred.rel.2015 <- apply(varMat1[,1:20],2,function(x) {x/max(x)})
V.pred.rel.2016 <- apply(varMat1[,21:40],2,function(x) {x/max(x)})
write.csv(V.pred.rel.2015,file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_name,'_varMat_2015.csv'), sep = '_')),row.names = FALSE)
write.csv(V.pred.rel.2016,file=file.path("Results/Uncertainty_partitioning",paste(site,paste0(model_name,'_varMat_2016.csv'), sep = '_')),row.names = FALSE)


#plot variances
dev.off(dev.list()["RStudioGD"])
plot_varMat(model_name = model_name)

## write stacked area plot to file
png(file=file.path(my_directory,paste(site,paste0(model_name,'_var_part.png'), sep = '_')), res=300, width=30, height=10, units='cm')
plot_varMat(model_name = model_name)
dev.off()


##looking at percentile of obs in forecast distribution
obs_quantile <- NULL
for (i in 1:length(forecast_ys)){
percentile1 <- ecdf(exp(forecast.IC.P.O[,i])) ##be sure to change this as needed - needs to be made into a function!!!
obs_quantile[i] <- percentile1(forecast_ys[i])
}
obs_quantile <- obs_quantile[-c(1,21)]

#should add vertical line at 0.5 to this
png(file=file.path(my_directory,paste(site,paste0(model_name,'_obs_quantile.png'), sep = '_')), res=300, width=10, height=10, units='cm')
hist(obs_quantile,xlab = "Quantile of obs. in forecast interval", main = "",
     cex.axis = 1.2, cex.lab = 1.2, xlim = c(0.2,1), breaks = seq(0,1,0.25))
dev.off()

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
