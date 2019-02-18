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
source('RCode/helper_functions/google_drive_functions.R')
source('RCode/NEFI/get_data.R')
source('RCode/helper_functions/plug_n_play_functions.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

start_date = '2007-01-01' # in YYYY-MM-DD format 
end_date = '2015-12-31' 
site = c('midge') # options are midge, coffin, newbury, or fichter 
model_timestep = 1 # model timestep in days if filling in dates
fill_dates = TRUE  # T/F for filling in dates w/o observations with NA's 

beta.m <- as.vector(c(0,0,0)) ##CHANGE THE NUMBER OF BETAS TO MATCH YOUR MODEL
beta.v <- solve(diag(1E-03,3)) ##CHANGE THE NUMBER OF BETAS TO MATCH YOUR MODEL

model_name = 'RandomWalk' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
model=paste0("RCode/NEFI/Jags_Models/",model_name, '.R') #Do not edit

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 200 


#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
dat = plug_n_play_data(start_date = start_date,
                       end_date = end_date,
                       sites = site,
                       model_timestep = model_timestep,
                       fill_dates = fill_dates)

##look at response variable 
y<-round(dat$totalperL*141.3707) #converting colonies per Liter to count data: volume of 2, ~1 m net tows
hist(y)  
N=length(y)
range(y, na.rm = T)



#3) JAGS Plug-Ins: Can run whole chunk or just model of interest --------------------------------------------------------------
jags_plug_ins <- jags_plug_ins(model_name = model_name,  
                               y = y, 
                               beta.m = beta.m, 
                               beta.v = beta.v, 
                               Temp = dat$watertemp_mean, 
                               DL = dat$daylength,
                               year_no = as.numeric(as.factor(dat$year))
)  

#4) Initial Conditions: We haven't set up an initial conditions except for tau_add, so don't change for now -------------------
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y[!is.na(y)],length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp)))
}


# Now that we've defined the model, the data, and the initialization, we need to send all this info to JAGS, which will return the JAGS model object.
# 
# Next, given the defined JAGS model, we'll want to take a few samples from the MCMC chain and assess when the model has converged. To take samples from the MCMC object we'll need to tell JAGS what variables to track and how many samples to take.
# 
# Since rjags returns the samples as a CODA object, we can use any of the diagnositics in the R *coda* library to test for convergence, summarize the output, or visualize the chains.

#5) Run model (no edits, unless you want to change # of iterations) -------------------------------------------------------------
j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = init,
                         n.chains = 3)

jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     adapt =  1000, 
                     burnin =  500, 
                     sample = 2500, 
                     n.chains = 3, 
                     inits=init,
                     monitor = jags_plug_ins$variable.namesout.model)

write.jagsfile(jags.out, file=file.path("Results/Jags_Models/",paste(site,paste0(model_name,'.txt'), sep = '_')), 
               remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

plot(jags.out, vars = jags_plug_ins$variable.names.model) 

jags.out.mcmc <- as.mcmc.list(jags.out)

out <- as.matrix(jags.out.mcmc)

DIC=dic.samples(j.model, n.iter=5000)
DIC


# Given the full joint posterior samples, we're next going to visualize the output by just looking at the 95% credible interval of the timeseries of X's and compare that to the observed Y's. To do so we'll convert the coda output into a matrix and then calculate the quantiles. Looking at colnames(out) will show you that the first two columns are `tau_add` and `tau_obs`, so we calculate the CI starting from the 3rd column. We also transform the samples back from the log domain to the linear domain.

#9) Obs. vs. Latent Variable CI Plots (no edits)

times <- as.Date(as.character(dat$date))
time.rng = c(1,length(times)) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=grep("mu", colnames(out))
mu = exp(out[,mus])
ci <- apply(exp(out[,mus]),2,quantile,c(0.025,0.5,0.975))

plot(times,ci[2,],type='n',ylim=range(y+.01,na.rm=TRUE), log="y", ylab="observed Gloeo colonies",xlim=times[time.rng])

ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.5)


#10) one step ahead prediction: plot out-of-sample predictions first because wide CIs; calculate means and plot predicted vs. observed

#Edit this section to match features of your model (change nsamp to be a subset of the total # of samples, change sample parameters & process model?)

#The default is just set to calculate for a random walk


#Calculate prediction intervals for one timestep ahead
#your latent states must be called "mu" in the model!

samp <- sample.int(nrow(out),nsamp)

# sample initial conditions
mus=grep("mu", colnames(out))
mu = out[samp,mus] 
times=c(1:length(mus))

# sample parameters 
# Edit to add other parameters from your model here
tau = out[samp,grep("tau",colnames(out))]

#Create matrix for predictions
pred <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){

  #Sample to get predictive interval for latent states based on process model
  
  #Edit this to reflect additional components of process model?
  pred[,t] = rnorm(nsamp,mu[,t-1],tau) #exponentiate these before comparing to data, because mu on log scale
  
  #exponentiate to get appropriate input for poisson
  m <- exp(pred[,t])
  
  #this fits the blended model to your observed data.
  pred_obs[,t] = rpois(nsamp, m)

}


#11) Diagnostic Visualization (no edits)

#Visualization

time.rng = c(1,length(times)) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
ylo[1])), border = NA,...) 
}
out <- as.matrix(jags.out.mcmc)
mus=grep("mu", colnames(out))
mu = exp(out[,mus])
ci <- apply(exp(out[,mus]),2,quantile,c(0.025,0.5,0.975))
pi <- apply(exp(pred),2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
obs_pi <- apply(pred_obs,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

plot(times,ci[2,],type='n',ylim=range(y+.01,na.rm=TRUE), log = "y", ylab="Gloeo count",xlim=times[time.rng])
ciEnvelope(times,obs_pi[1,]+0.0001,obs_pi[3,],col="gray")
ciEnvelope(times,pi[1,],pi[3,],col="Green")
ciEnvelope(times,ci[1,],ci[3,],col="lightBlue")
points(times,y,pch="+",cex=0.5)


#12) Further Diagnostic Checks and Visualization (no edits)

#y vs. preds

obs_diff= vector(mode="numeric", length=0)
obs_quantile = vector(mode="numeric", length=0)
obs_quantile_dm = vector(mode="numeric",length=0)
pred_mean = vector(mode="numeric",length=0)

for(i in 2:ncol(pred)){
  obs_diff[i]=mean(exp(pred[,i]))-y[i] #difference between mean of pred. values and obs for each time point
  pred_mean[i]=mean(exp(pred[,i])) #mean of pred. values at each time point
  percentile <- ecdf(exp(pred[,i])) #create function to give percentile based on distribution of pred. values at each time point
  obs_quantile[i] <- percentile(y[i]) #get percentile of obs in pred distribution
  percentile1 <- ecdf(pred_obs[,i]) #create function to give percentile of obs in distribution of pred including observation error
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


#13) Plots (no edits)

#hist of quantiles
hist(obs_quantile) #no observation error
hist(obs_quantile_dm, breaks = seq(0,1,0.05)) #with observation error

#plot of mean pred vs. obs
plot(y,pred_mean, xlim = c(0,500), ylim = c(0,500)) #no obs error

## qqplot - plot of quantiles of data in distribution including obs error
plot(seq(0,1,length.out = length(obs_quantile_dm)-1),sort(obs_quantile_dm),
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

