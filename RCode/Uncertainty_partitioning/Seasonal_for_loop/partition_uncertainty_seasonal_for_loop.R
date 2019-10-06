# uncertainty partitioning 


# State-space model - revised by S. LaDeau (11/2017) from the EcoForecast Activity by Michael Dietze, with reference "Ecological Forecasting", chapter 8
# ========================================================
#   
#   The data used for this example are from summer weekly(ish) Gloetrichia echinulata (Gloeo.) sampling at 4 locations in Lake Sunapee, NH. The data are provided by Kathryn Cottingham, and should not be used without permission outside this workshop.

library(tidyverse)
library(readxl)
library(rjags)
library(runjags)
library(moments)
library(geosphere)
library(googledrive)
source('RCode/Helper_functions/seasonal_plug_n_play.R')
source('RCode/Helper_functions/forecast_plug_n_play.R')

#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------
model_name = 'Seasonal_RandomWalk' #pick a model name
model=paste0("RCode/Jags_Models/Seasonal_for_loop/",model_name, '.R') #this is the folder where your models are stored

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 1500 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning"

#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_22JUL19.csv"))+0.003)
forecast_y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
forecast_y <- forecast_y[7:8,]

#for watertemp_mean
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_11AUG19.csv"))
Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_16AUG19.csv"))

#for airtemp
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_airtemp_22JUL19.csv"))

#for Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_Schmidt_11AUG19.csv"))


years <- c(2009:2014)
forecast_years <- c(2015:2016)
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)
site = "Midge"

#for water temp
week_avg = colMeans(Temp_prior, na.rm = TRUE)

#for Schmidt
week_avg = colMeans(Schmidt, na.rm = TRUE)

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

######## deterministic prediction #######
##Set up forecast
settings.det <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                 Nmc = 1,
                 IC = cbind(-5,-5))

params.det <- list(sd_obs = 0, #note these are SDs, not taus!!
                   sd_proc = 0)

#Run forecast
det.prediction <- forecast_gloeo(model_name = model_name,
                           params = params.det,
                           settings = settings.det)

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

params.IC <- list(sd_obs = 0,
                   sd_proc = 0)


#Run forecast
forecast.IC <- forecast_gloeo(model_name = model_name,
                                 params = params.IC,
                                 settings = settings.IC)

## Plot
forecast.ci.IC = apply(exp(forecast.IC), 2, quantile, c(0.025,0.5,0.975))

forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y",
              forecast.ci = forecast.ci.IC) #choose from "y" or "n"

#just a couple of checks to make sure this is behaving as expected
hist(exp(IC[,1]))
hist(exp(forecast.IC[,40]))

###### parameter uncertainty #######
# we don't have this for random walk 

###### driver uncertainty ########## 
# we don't have this for random walk 

###### process uncertainty ######### 
prow = sample.int(nrow(out),Nmc,replace=TRUE)
sd_proc <- 1/sqrt(out[prow,"tau_proc"])  ## convert from precision to standard deviation

##Set up forecast
settings.IC.P <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                    Nmc = Nmc,
                    IC = IC)

params.IC.P <- list(sd_obs = 0,
                  sd_proc = sd_proc)


#Run forecast
forecast.IC.P <- forecast_gloeo(model_name = model_name,
                              params = params.IC.P,
                              settings = settings.IC.P)

## Plot
forecast.ci.IC.P = apply(exp(forecast.IC.P), 2, quantile, c(0.025,0.5,0.975))

forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y",
              forecast.ci = forecast.ci.IC.P) #choose from "y" or "n"

#just a couple of checks to make sure this is behaving as expected
hist(exp(forecast.IC.P[,10]))


###### observation uncertainty ######### 
sd_obs <- 1/sqrt(out[prow,"tau_obs"])  ## convert from precision to standard deviation

##Set up forecast
settings.IC.P.O <- list(N_out = 40, #length of forecast time points (2 years x 20 weeks)
                      Nmc = Nmc,
                      IC = IC)

params.IC.P.O <- list(sd_obs = sd_obs,
                    sd_proc = sd_proc)


#Run forecast
forecast.IC.P.O <- forecast_gloeo(model_name = model_name,
                                params = params.IC.P.O,
                                settings = settings.IC.P.O)

## Plot
forecast.ci.IC.P.O = apply(exp(forecast.IC.P.O), 2, quantile, c(0.025,0.5,0.975))

forecast_plot(cal_years = c(2009:2014), 
              forecast_years = c(2015:2016), 
              is.forecast.ci  = "y",
              forecast.ci = forecast.ci.IC.P.O) #choose from "y" or "n"

### calculation of variances
var.IC     <- apply(forecast.IC,2,var)
var.IC.P    <- apply(forecast.IC.P,2,var)
var.IC.P.O   <- apply(forecast.IC.P.O,2,var)
varMat   <- rbind(var.IC,var.IC.P,var.IC.P.O)
V.pred.rel.2015 <- apply(varMat[,1:20],2,function(x) {x/max(x)})
V.pred.rel.2016 <- apply(varMat[,21:40],2,function(x) {x/max(x)})
V.pred.rel <- (V.pred.rel.2015 + V.pred.rel.2016) / 2


## stacked area plot
N.cols <- c("black","red","green","blue","orange") ## set colors
png(file=file.path(my_directory,paste(site,paste0(model_name,'_var_part.png'), sep = '_')), res=300, width=15, height=10, units='cm')
plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season")
ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
legend("bottomright", legend=c("Initial Cond","Process","Observation"), col=N.cols[1:3], lty=1, lwd=3, bg = 'white', cex = 0.8)
dev.off()

