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
source('RCode/helper_functions/seasonal_plug_n_play.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

model_name = 'Seasonal_RandomWalk_Obs_error' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
model=paste0("RCode/Jags_Models/Seasonal_for_loop/",model_name, '.R') #Do not edit

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 1500 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Desktop/MEL_Bayes/Temperature_obs_error_seasonal"


#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
y <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_22JUL19.csv"))

Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_airtemp_22JUL19.csv"))

years <- c(2009:2014)
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)
site = "Midge"


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
                     adapt =  10000, 
                     burnin =  20000, 
                     sample = 5000, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#5) Save and Process Output
write.jagsfile(jags.out, file=file.path("Results/Jags_Models/",paste(site,paste0(model_name,'.txt'), sep = '_')), 
               remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

#this will create multiple plots if var names are different but doesn't create multiple
#plots for vars with same names, i.e., betas, so have created a quick hack to fix that

#ok, you have to edit this vector to include the actual names of the parameters in your model :(
params <- jags_plug_ins$params.model

for (i in 1:length(params)){
  #png(file=file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
  plot(jags.out, vars = params[i]) 
  #dev.off()
}

#upload plot to Google Drive folder
for (i in 1:length(params)){
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
}
#need to view this to get parameter estimates for model comparison Excel file
sum <- summary(jags.out, vars = jags_plug_ins$variable.names.model)
sum

jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

DIC=dic.samples(j.model, n.iter=5000)
DIC

#Seasonal_RandomWalk_Obs_error

#Seasonal_Temperature_Obs_error
# Mean deviance:  -407.1 
# penalty 22139 
# Penalized deviance: 21732 

saveRDS(object = DIC, file = file.path("Results/Jags_Models/", paste(site, paste0(model_name,'_DIC.rds'), sep = '_')))


#6) CI, PI, Obs PI Calculations

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  filter(site == "midge")

time <- as.Date(as.character(dat$date))
times <- time[101:120]
#time.rng = c(1,20) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=grep("mu\\[6,", colnames(out))
mu = out[,mus]
ci <- apply(mu,2,quantile,c(0.025,0.5,0.975))

## One step ahead prediction intervals

samp <- sample.int(nrow(out),nsamp)
mus=grep("mu\\[6,", colnames(out))
mu = out[samp,mus] 
Temps=Temp[6,]

#Temperature_obs_error

  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  sd_obs = out[samp,grep("sd_obs",colnames(out))]
  yr_temp = out[samp,grep("yr",colnames(out))]
  yr=yr_temp[,-1]
  
  pred.Temperature_obs_error_seasonal <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Temperature_obs_error_seasonal <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  
  for (k in 1:length(year_no)){
  for (t in 2:ncol(mu)){
    lambda[,t] <- beta1 + beta2*mu[,t-1] + beta3*Temps[t] + yr[,year_no[k]]
    pred.Temperature_obs_error_seasonal[,t] = rnorm(nsamp, lambda[,t], tau_proc) 
    m <- pred.Temperature_obs_error_seasonal[,t] 
    pred_obs.Temperature_obs_error_seasonal[,t] = rnorm(nsamp, m, 1/sd_obs^2)}}

  pred_obs= eval(parse(text = paste0('pred_obs.', model_name)))
  pred = eval(parse(text = paste0('pred.', model_name)))
  
  preds_plug_ins <- list(pred_obs.model = pred_obs, pred.model = pred) 

pi <- apply(preds_plug_ins$pred.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
obs_pi <- apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)


#7) CI, PI, Obs PI Plots


#CI, PI, Obs PI
png(file=file.path(my_directory,paste(site,paste0(model_name,'_CI_PI_2014.png'), sep = '_')), res=300, width=15, height=20, units='cm')
plot(times,ci[2,],type='n', ylab="Gloeo count", ylim = c(min(ci[1,]),max(ci[3,])),main="Obs, Latent CI (blue), PI (green), Obs PI (grey)")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
ciEnvelope(times,obs_pi[1,],obs_pi[3,],col="gray")
ciEnvelope(times,pi[1,],pi[3,],col="Green")
points(times,y[6,],pch="+",cex=0.8)
points(times,obs_pi[2,],pch = 5, cex = 0.8)
dev.off()



# #upload plot to Google Drive folder
# drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')),
#              path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')))



#8) Further Diagnostic Checks and Visualization 

#y vs. preds

obs_diff= vector(mode="numeric", length=0)
obs_quantile = vector(mode="numeric", length=0)
obs_quantile_dm = vector(mode="numeric",length=0)
pred_mean = vector(mode="numeric",length=0)

for(i in 2:ncol(preds_plug_ins$pred.model)){
  obs_diff[i]=mean(preds_plug_ins$pred.model[,i])-y[1,i] #difference between mean of pred. values and obs for each time point
  pred_mean[i]=mean(preds_plug_ins$pred.model[,i]) #mean of pred. values at each time point
  percentile <- ecdf(preds_plug_ins$pred.model[,i]) #create function to give percentile based on distribution of pred. values at each time point
  obs_quantile[i] <- percentile(y[1,i]) #get percentile of obs in pred distribution
  percentile1 <- ecdf(preds_plug_ins$pred_obs.model[,i]) #create function to give percentile of obs in distribution of pred including observation error
  obs_quantile_dm[i] <- percentile1(y[1,i]) #get percentile of obs 
}

#Mean of difference between pred and obs
obspred_mean=mean(obs_diff, na.rm=TRUE)
obspred_mean

#Mean quantile of obs in distribution of pred
obs_quantile_mean = mean(obs_quantile, na.rm = TRUE)
obs_quantile_mean

#Mean quantile of obs in distribution of pred including observation error
obs_quantile_mean_dm = mean(obs_quantile_dm, na.rm = TRUE)
obs_quantile_mean_dm


#9) Diagnostic Plots

# png(file=file.path(my_directory,paste(site,paste0(model_name,'_Diagnostics.png'), sep = '_')), res=300, width=15, height=22, units='cm')
# par(mfrow=c(3,2))

#hist of quantiles
hist(obs_quantile, main="No obs error") #no observation error
hist(obs_quantile_dm, breaks = seq(0,1,0.05), main="With obs error") #with observation error

#plot of mean pred vs. obs
plot(y[1,],pred_mean, main="Mean pred vs. obs, no obs error") #no obs error

## qqplot - plot of quantiles of data in distribution including obs error
plot(seq(0,1,length.out = length(sort(obs_quantile_dm))),sort(obs_quantile_dm), main="QQplot",
xlab = "Theoretical Quantile",
ylab = "Empirical Quantile")
abline(0,1)

######STOPPED ADAPTING HERE

## time series 
date=as.character(dat$date)
dates<-as.Date(date)
par(mar = c(5,3,4,3))
plot(dates, obs_quantile_dm,main = "dots = obs. quantiles, triangles = gloeo counts",ylab = "")
mtext("obs. quantile", side=2, line=1.7)
par(new = TRUE)
plot(dates, y, axes = FALSE, bty = "n", xlab = "", ylab = "",pch = 17, col = "red", cex = 0.8)
axis(side=4, at = pretty(range(y)))
mtext("gloeo counts", side=4, line=1.6)

dev.off()

#once again, upload plot to Google Drive folder
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_Diagnostics.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_Diagnostics.png'), sep = '_')))

