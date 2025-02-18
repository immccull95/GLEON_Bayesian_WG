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
library(ecoforecastR)
library(googledrive)
source('RCode/helper_functions/google_drive_functions.R')
source('RCode/NEFI/get_data.R')
source('RCode/helper_functions/plug_n_play_functions.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

start_date = '2007-01-01' # in YYYY-MM-DD format; 1st 
end_date = '2015-12-31' # Excluding 2016-2017 to use as sample data
site = c('midge') # options are midge, coffin, newbury, or fichter 
model_timestep = 7 # model timestep in days if filling in dates
fill_dates = TRUE  # T/F for filling in dates w/o observations with NA's 
forecast = TRUE # T/F for returing forecast time period or not 
forecast_end_date = '2017-12-31' 

model_name = 'DayLengthQuad' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
model=paste0("RCode/NEFI/Jags_Models/",model_name, '.R') #Do not edit

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 500 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Desktop/MEL_Bayes"

#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
dat = plug_n_play_data(start_date = start_date,
                       end_date = end_date,
                       sites = site,
                       model_timestep = model_timestep,
                       forecast = forecast, 
                       forecast_end_date = forecast_end_date,
                       fill_dates = fill_dates)

##look at response variable 
y<-round(dat$cal$totalperL*141.3707) #converting colonies per Liter to count data: volume of 2, ~1 m net tows
hist(y)  
N=length(y)
range(y, na.rm = T)

Temp = dat$cal$watertemp_mean
DL = c(dat$cal$daylength)
year_no = as.numeric(as.factor(dat$cal$year))


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
                     adapt =  2000, 
                     burnin =  500, 
                     sample = 2500, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#5) Save and Process Output

plot(jags.out, vars = jags_plug_ins$variable.names.model) 

jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

# DIC=dic.samples(j.model, n.iter=5000)
# DIC


#6) CI, PI, Obs PI Calculations

times <- as.Date(as.character(dat$cal$date))
time.rng = c(1,length(times)) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=grep("mu", colnames(out))
mu = exp(out[,mus])
ci <- apply(exp(out[,mus]),2,quantile,c(0.025,0.5,0.975))
preds_plug_ins <- preds_plug_ins(model_name = model_name)
pi <- apply(exp(preds_plug_ins$pred.model),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
obs_pi <- apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)


#7) CI, PI, Obs PI Plots

#Obs vs. Latent
plot(times,ci[2,],type='n',ylim=range(y+.01,na.rm=TRUE), log="y", ylab="observed Gloeo colonies",xlim=times[time.rng], main="Obs, Latent CI")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.5)

#CI, PI, Obs PI
plot(times,ci[2,],type='n',ylim=range(y+.01,na.rm=TRUE), log = "y", ylab="Gloeo count",xlim=times[time.rng], main="Obs, Latent CI (blue), PI (green), Obs PI (grey)")
ciEnvelope(times,obs_pi[1,]+0.0001,obs_pi[3,],col="gray")
ciEnvelope(times,pi[1,],pi[3,],col="Green")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.5)


## Forward Simulation

### settings
Nmc = 1000         ## set number of Monte Carlo draws
N.cols <- c("black","red","green","blue","orange") ## set colors
ylim = range(ci+0.01, na.rm = TRUE)
trans <- 0.8       ## set transparancy
cal_time = as.Date(as.character(dat$cal$date))
forecast_time = as.Date(as.character(dat$forecast$date))
all_time = c(cal_time, forecast_time)

plot.run <- function(){
  plot(all_time, all_time, type='n', ylim=ylim, ylab="Gloeo count", log = 'y')
  axis(2)
  ecoforecastR::ciEnvelope(cal_time, ci[1,], ci[3,], col="lightBlue")
  lines(cal_time, ci[2,], col="purple")
  points(cal_time, ci[2,])
}

#ci <- apply(exp(out[ ,mus]), 2, quantile, c(0.025,0.5,0.975))
plot.run()


#setting up deterministic forecast function for random walk 
# IC = initial conditions from end of calibration period 
# N_out = number of time steps to forecast 
# tau_add = process error (defaults to zero) 
# n = number of monte carlo sims 
forecast_gloeo <- function(IC, beta1, beta2, beta3, beta4, DL=DL, N_out = N_out, tau_add = 0, n = Nmc){
  out <- matrix(NA, n, N_out) # setting up output 
  gloeo_prev <- IC
  for(i in 1:N_out){
    lambda <- beta1 + beta2*gloeo_prev + beta3*DL[i] + beta4*DL[i]^2
    out[,i] <- rnorm(n,lambda,tau_add) 
    gloeo_prev <- out[,i] # update IC 
  }
  return(out)
}

## deterministic predictions 
DL = c(dat$forecast$daylength) #DL during forecasted period
N_out = nrow(dat$forecast) # length of forecast time points 
IC = out[,mus]
betas = out[,grep("beta",colnames(out))]
betas.mean <- apply(betas,2,mean)

gloeo.det <- forecast_gloeo(IC = mean(IC[,N]), # last column of cal is the initial condition for forecast 
                            N_out = N_out,
                            beta1 = betas.mean[1],
                            beta2 = betas.mean[2],
                            beta3 = betas.mean[3],
                            beta4 = betas.mean[4],
                            DL = DL,
                            tau_add = 0,
                            n = 1)

## Plot run
plot.run()
lines(forecast_time, exp(gloeo.det), col="purple", lwd=3)


######## initial condition uncertainty #######
# sample rows from previous analysis
prow = sample.int(nrow(out),Nmc,replace=TRUE)

gloeo.I <- forecast_gloeo(IC = IC[prow, N],
                      N_out = N_out,
                      tau_add = 0,
                      beta1 = betas.mean[1],
                      beta2 = betas.mean[2],
                      beta3 = betas.mean[3],
                      beta4 = betas.mean[4],
                      DL = DL,
                      n = Nmc)
gloeo.I.ci = apply(exp(gloeo.I), 2, quantile, c(0.025,0.5,0.975))

## Plot run
plot.run()
ecoforecastR::ciEnvelope(forecast_time, gloeo.I.ci[1,], gloeo.I.ci[3,], col = col.alpha(N.cols[1], trans))
lines(forecast_time, gloeo.I.ci[2,], lwd=0.5)

###### parameter uncertainty #######
gloeo.IP <- forecast_gloeo(IC=IC[prow,N],  ## shouldn't I be holding IC constant tho??
                      N_out = N_out,
                      beta1 = betas[prow,1],
                      beta2 = betas[prow,2],
                      beta3 = betas[prow,3],
                      beta4 = betas[prow,4],
                      DL = DL,
                      tau_add=0,
                      n=Nmc)

## Plot run
plot.run()
gloeo.IP.ci = apply(exp(gloeo.IP),2,quantile,c(0.025,0.5,0.975))
ecoforecastR::ciEnvelope(forecast_time,gloeo.IP.ci[1,],gloeo.IP.ci[3,],col=col.alpha(N.cols[2],trans))
ecoforecastR::ciEnvelope(forecast_time,gloeo.I.ci[1,],gloeo.I.ci[3,],col=col.alpha(N.cols[1],trans))
lines(forecast_time,gloeo.I.ci[2,],lwd=0.5)

###### driver uncertainty ########## 
# we don't have this for exponential 

###### process uncertainty ######### 
tau_add_mc <- 1/sqrt(out[prow,"tau_add"])  ## convert from precision to standard deviation

gloeo.IPE <- forecast_gloeo(IC = IC[prow, N],
                          N_out = N_out,
                          beta1 = betas[prow,1],
                          beta2 = betas[prow,2],
                          beta3 = betas[prow,3],
                          beta4 = betas[prow,4],
                          DL = DL,
                          tau_add = tau_add_mc,
                          n = Nmc)
gloeo.IPE.ci = apply(exp(gloeo.IPE), 2, quantile, c(0.025,0.5,0.975))

## Plot run
png(file=file.path(my_directory,paste(site,paste0(model_name,'_forecast.png'), sep = '_')), res=300, width=15, height=10, units='cm')
plot.run()
ecoforecastR::ciEnvelope(forecast_time, gloeo.IPE.ci[1,], gloeo.IPE.ci[3,], col = col.alpha(N.cols[4], trans))
ecoforecastR::ciEnvelope(forecast_time, gloeo.IP.ci[1,], gloeo.IP.ci[3,], col = col.alpha(N.cols[2], trans))
ecoforecastR::ciEnvelope(forecast_time, gloeo.I.ci[1,], gloeo.I.ci[3,], col = col.alpha(N.cols[1], trans))
lines(forecast_time, exp(gloeo.det), col="purple", lwd=3)
dev.off()

#plot to show total uncertainty for poster
png(file=file.path(my_directory,paste(site,paste0(model_name,'_tot_uncert.png'), sep = '_')), res=300, width=15, height=10, units='cm')
plot.run()
ecoforecastR::ciEnvelope(forecast_time, gloeo.IPE.ci[1,], gloeo.IPE.ci[3,], col = "thistle1")
ecoforecastR::ciEnvelope(forecast_time, gloeo.IP.ci[1,], gloeo.IP.ci[3,], col = "thistle1")
ecoforecastR::ciEnvelope(forecast_time, gloeo.I.ci[1,], gloeo.I.ci[3,], col = "thistle1")
lines(forecast_time, exp(gloeo.det), col="purple", lwd=3)
dev.off()

#upload to Drive
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_forecast.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_forecast.png'), sep = '_')))


### calculation of variances
varI     <- apply(gloeo.I,2,var)
varIP    <- apply(gloeo.IP,2,var)
varIPE   <- apply(gloeo.IPE,2,var)
varMat   <- rbind(varI,varIP,varIPE)
V.pred.rel <- apply(varMat,2,function(x) {x/max(x)})


## stacked area plot
png(file=file.path(my_directory,paste(site,paste0(model_name,'_var_part.png'), sep = '_')), res=300, width=15, height=10, units='cm')
plot(forecast_time, V.pred.rel[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="2016")
ciEnvelope(forecast_time, rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
ciEnvelope(forecast_time, V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
ciEnvelope(forecast_time, V.pred.rel[2,], V.pred.rel[3,], col = N.cols[4])
legend("topleft", legend=c("Initial Cond","Parameter", "Process"), col=(N.cols[c(1,2,4)]), lty=1, lwd=5, bg = 'white')
dev.off()

drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_var_part.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_var_part.png'), sep = '_')))
