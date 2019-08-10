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
library(truncnorm)
source('RCode/helper_functions/seasonal_plug_n_play.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

model_name = 'Seasonal_RandomWalk_Obs_error' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
model=paste0("RCode/Jags_Models/Seasonal_for_loop/",model_name, '.R') #Do not edit

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 1500 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Documents/Ch5/Prelim_results_10AUG19"


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
                     burnin =  50000, 
                     sample = 10000, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#5) Save and Process Output
write.jagsfile(jags.out, file=file.path("Results/Jags_Models/Seasonal_for_loop",paste(site,paste0(model_name,'.txt'), sep = '_')), 
               remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

#this will create multiple plots if var names are different but doesn't create multiple
#plots for vars with same names, i.e., betas, so have created a quick hack to fix that

#ok, you have to edit this vector to include the actual names of the parameters in your model :(
params <- jags_plug_ins$params.model

for (i in 1:length(params)){
  png(file=file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
  plot(jags.out, vars = params[i]) 
  dev.off()
}

#upload plot to Google Drive folder
for (i in 1:length(params)){
drive_upload(file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')),
             path = file.path("./GLEON_Bayesian_WG/Model_diagnostics/Seasonal_for_loop",paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
}
#need to view this to get parameter estimates for model comparison Excel file
sum <- summary(jags.out, vars = jags_plug_ins$variable.names.model)
DIC=dic.samples(j.model, n.iter=5000)

#save results
sink(file = file.path("Results/Jags_Models/Seasonal_for_loop",paste(site,paste0(model_name,'_param_summary.txt'), sep = '_')))
print("Parameter summary")
sum
print("DIC")
DIC
sink()

jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

#Seasonal_RandomWalk_Obs_error
saveRDS(object = DIC, file = file.path("Results/Jags_Models/Seasonal_for_loop", paste(site, paste0(model_name,'_DIC.rds'), sep = '_')))


#6) CI, PI, Obs PI Calculations

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  filter(site == "midge")

times <- as.Date(as.character(dat$date))
#time.rng = c(1,20) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=c(grep("mu\\[1,", colnames(out)),grep("mu\\[2,", colnames(out)),
      grep("mu\\[3,", colnames(out)),grep("mu\\[4,", colnames(out)),
      grep("mu\\[5,", colnames(out)),grep("mu\\[6,", colnames(out)))
mu = out[,mus]
ci <- apply(mu,2,quantile,c(0.025,0.5,0.975))

## One step ahead prediction intervals

samp <- sample.int(nrow(out),nsamp)
mu = out[samp,mus] 
Temps=c(Temp[1,], Temp[2,], Temp[3,], Temp[4,], Temp[5,], Temp[6,])
ys = c(y[1,],y[2,],y[3,],y[4,],y[5,],y[6,])

#Temperature_obs_error

  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
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
  
  preds_plug_ins <- preds_plug_ins(model_name) 

pi <- apply(preds_plug_ins$pred.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
obs_pi <- apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)


#7) CI, PI, Obs PI Plots


#CI, PI, Obs PI
png(file=file.path(my_directory,paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')), res=300, width=20, height=15, units='cm')
par(mfrow = c(3,2), oma = c(1,1,5,1), mar = c(4,4,2,2)+0.1)

#2009
plot(times[1:20],ci[2,1:20],type='n', ylab="Gloeo count", ylim = c(min(ci[1,1:20]),max(ci[3,1:20])),
     main="",xlab = "")
ciEnvelope(times[1:20],obs_pi[1,1:20],obs_pi[3,1:20],col="gray")
ciEnvelope(times[1:20],pi[1,1:20],pi[3,1:20],col="Green")
ciEnvelope(times[1:20],ci[1,1:20],ci[3,1:20],col="lightBlue")
points(times[1:20],ys[1:20],pch="+",cex=0.8)
legend("topleft",legend = "2009", bty = "n")
#points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2010
plot(times[21:40],ci[2,21:40],type='n', ylab="Gloeo count", ylim = c(min(ci[1,21:40]),max(ci[3,21:40])),
     main="",xlab = "")
ciEnvelope(times[21:40],obs_pi[1,21:40],obs_pi[3,21:40],col="gray")
ciEnvelope(times[21:40],pi[1,21:40],pi[3,21:40],col="Green")
ciEnvelope(times[21:40],ci[1,21:40],ci[3,21:40],col="lightBlue")
points(times[21:40],ys[21:40],pch="+",cex=0.8)
legend("topleft",legend = "2010", bty = "n")
#points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2011
plot(times[41:60],ci[2,41:60],type='n', ylab="Gloeo count", ylim = c(min(ci[1,41:60]),max(ci[3,41:60])),
     main="",xlab = "")
ciEnvelope(times[41:60],obs_pi[1,41:60],obs_pi[3,41:60],col="gray")
ciEnvelope(times[41:60],pi[1,41:60],pi[3,41:60],col="Green")
ciEnvelope(times[41:60],ci[1,41:60],ci[3,41:60],col="lightBlue")
points(times[41:60],ys[41:60],pch="+",cex=0.8)
legend("topleft",legend = "2011", bty = "n")
#points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2012
plot(times[61:80],ci[2,61:80],type='n', ylab="Gloeo count", ylim = c(min(ci[1,61:80]),max(ci[3,61:80])),
     main="",xlab = "")
ciEnvelope(times[61:80],obs_pi[1,61:80],obs_pi[3,61:80],col="gray")
ciEnvelope(times[61:80],pi[1,61:80],pi[3,61:80],col="Green")
ciEnvelope(times[61:80],ci[1,61:80],ci[3,61:80],col="lightBlue")
points(times[61:80],ys[61:80],pch="+",cex=0.8)
legend("topleft",legend = "2012", bty = "n")
#points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2013
plot(times[81:100],ci[2,81:100],type='n', ylab="Gloeo count", ylim = c(min(ci[1,81:100]),max(ci[3,81:100])),
     main="",xlab = "")
ciEnvelope(times[81:100],obs_pi[1,81:100],obs_pi[3,81:100],col="gray")
ciEnvelope(times[81:100],pi[1,81:100],pi[3,81:100],col="Green")
ciEnvelope(times[81:100],ci[1,81:100],ci[3,81:100],col="lightBlue")
points(times[81:100],ys[81:100],pch="+",cex=0.8)
legend("topleft",legend = "2013", bty = "n")
#points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2014
plot(times[101:120],ci[2,101:120],type='n', ylab="Gloeo count", ylim = c(min(ci[1,101:120]),max(ci[3,101:120])),
     main="",xlab = "")
ciEnvelope(times[101:120],obs_pi[1,101:120],obs_pi[3,101:120],col="gray")
ciEnvelope(times[101:120],pi[1,101:120],pi[3,101:120],col="Green")
ciEnvelope(times[101:120],ci[1,101:120],ci[3,101:120],col="lightBlue")
points(times[101:120],ys[101:120],pch="+",cex=0.8)
legend("topleft",legend = "2014", bty = "n")
#points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

title(main="Obs (+), Latent CI (blue), PI (green), Obs PI (grey)",outer=T) 


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

