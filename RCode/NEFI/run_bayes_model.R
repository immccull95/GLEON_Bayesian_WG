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
source('RCode/helper_functions/google_drive_functions.R')
source('RCode/NEFI/get_data.R')
source('RCode/helper_functions/plug_n_play_functions.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

start_date = '2007-01-01' # in YYYY-MM-DD format; 1st 
end_date = '2015-12-31' # Excluding 2016-2017 to use as sample data
site = c('midge') # options are midge, coffin, newbury, or fichter 
model_timestep = 7 # model timestep in days if filling in dates
fill_dates = FALSE  # T/F for filling in dates w/o observations with NA's 

model_name = 'Temperature_obs_error' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
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
                       fill_dates = fill_dates)
#dat <- dat %>% filter(!is.na(totalperL))

##look at response variable 
y<-round(dat$totalperL*141.3707) #converting colonies per Liter to count data: volume of 2, ~1 m net tows
hist(y)  
N=length(y)
range(y, na.rm = T)

Temp = dat$TOBS
DL = dat$daylength
year_no = as.numeric(as.factor(dat$year))
season_weeks = length(c(min(dat$season_week):max(dat$season_week)))


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
                     burnin =  5000, 
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
vars <- c("tau_proc","beta1", "beta2", "beta3", "tau_yr","sd_obs")

for (i in 1:length(vars)){
  #png(file=file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',vars[i],'.png'), sep = '_')))
  plot(jags.out, vars = vars[i]) 
  #dev.off()
}

#upload plot to Google Drive folder
for (i in 1:length(vars)){
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',vars[i],'.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_Convergence_',vars[i],'.png'), sep = '_')))
}
#need to view this to get parameter estimates for model comparison Excel file
sum <- summary(jags.out, vars = jags_plug_ins$variable.names.model)
sum

jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

DIC=dic.samples(j.model, n.iter=5000)
DIC

saveRDS(object = DIC, file = file.path("Results/Jags_Models/", paste(site, paste0(model_name,'_DIC.rds'), sep = '_')))


#6) CI, PI, Obs PI Calculations

times <- as.Date(as.character(dat$date))
time.rng = c(79,98) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=grep("mu", colnames(out))
mu = out[,mus]
ci <- apply(mu,2,quantile,c(0.1,0.5,0.9))
preds_plug_ins <- preds_plug_ins(model_name = model_name)
pi <- apply(preds_plug_ins$pred.model,2,quantile,c(0.1,0.5,0.9), na.rm=TRUE)
obs_pi <- apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.1,0.5,0.9), na.rm=TRUE)


#7) CI, PI, Obs PI Plots

png(file=file.path(my_directory,paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')), res=300, width=15, height=20, units='cm')
par(mfrow=c(2,1))

#Obs vs. Latent
plot(times,ci[2,],type='n',ylim=range(y+.01,na.rm=TRUE), log="y", ylab="observed Gloeo colonies",xlim=times[time.rng], main="Obs, Latent CI")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.5)

#CI, PI, Obs PI
plot(times,ci[2,],type='n', ylab="Gloeo count",xlim=times[time.rng], main="Obs, Latent CI (blue), PI (green), Obs PI (grey)")
ciEnvelope(times,obs_pi[1,],obs_pi[3,],col="gray")
ciEnvelope(times,pi[1,],pi[3,],col="Green")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.8)
points(times,obs_pi[2,],pch = 5, cex = 0.8)

plot(times,ci[2,],type='n',ylim=range(y+.01,na.rm=TRUE), log = "y", ylab="Gloeo count",xlim=times[time.rng], main="Obs, Latent CI (blue), PI (green), Obs PI (grey)")
ciEnvelope(times,obs_pi[1,]+0.0001,obs_pi[3,],col="gray")
ciEnvelope(times,pi[1,],pi[3,],col="Green")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.5)


dev.off()

#upload plot to Google Drive folder
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')))

#same but for a single year (2010)
time.rng = c(182,216)

png(file=file.path(my_directory,paste(site,paste0(model_name,'_CI_PI_2010.png'), sep = '_')), res=300, width=15, height=20, units='cm')
par(mfrow=c(2,1))

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


dev.off()

#upload plot to Google Drive folder
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_CI_PI_2010.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics",paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')))


#8) Further Diagnostic Checks and Visualization 

#y vs. preds

obs_diff= vector(mode="numeric", length=0)
obs_quantile = vector(mode="numeric", length=0)
obs_quantile_dm = vector(mode="numeric",length=0)
pred_mean = vector(mode="numeric",length=0)

for(i in 2:ncol(preds_plug_ins$pred.model)){
  obs_diff[i]=mean(exp(preds_plug_ins$pred.model[,i]))-y[i] #difference between mean of pred. values and obs for each time point
  pred_mean[i]=mean(exp(preds_plug_ins$pred.model[,i])) #mean of pred. values at each time point
  percentile <- ecdf(exp(preds_plug_ins$pred.model[,i])) #create function to give percentile based on distribution of pred. values at each time point
  obs_quantile[i] <- percentile(y[i]) #get percentile of obs in pred distribution
  percentile1 <- ecdf(preds_plug_ins$pred_obs.model[,i]) #create function to give percentile of obs in distribution of pred including observation error
  obs_quantile_dm[i] <- percentile1(y[i]) #get percentile of obs 
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

png(file=file.path(my_directory,paste(site,paste0(model_name,'_Diagnostics.png'), sep = '_')), res=300, width=15, height=22, units='cm')
par(mfrow=c(3,2))

#hist of quantiles
hist(obs_quantile, main="No obs error") #no observation error
hist(obs_quantile_dm, breaks = seq(0,1,0.05), main="With obs error") #with observation error

#plot of mean pred vs. obs
plot(y,pred_mean, xlim = c(0,500), ylim = c(0,500), main="Mean pred vs. obs, no obs error") #no obs error

## qqplot - plot of quantiles of data in distribution including obs error
plot(seq(0,1,length.out = length(sort(obs_quantile_dm))),sort(obs_quantile_dm), main="QQplot",
xlab = "Theoretical Quantile",
ylab = "Empirical Quantile")
abline(0,1)

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

