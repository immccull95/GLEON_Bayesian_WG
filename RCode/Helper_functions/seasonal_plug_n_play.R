# plug and play scripts 
# JAZ 2019-02-15
# WB Updates
# MEL updates for seasonal for-loop 30JUL19

jags_plug_ins <- function(model_name){

#JAGS Plug-ins: Add each separate model here 
#variable.names are variables you would like to plot for model convergence (e.g., excludes mu)
#variable.names.out are all variables you would like to monitor in the jags run 
#init are a range of initial conditions for parameters in each of 3 chains 

#Seasonal_RandomWalk_Obs_error 
  data.Seasonal_RandomWalk_Obs_error <- list(y=y, year_no = year_no,season_weeks=season_weeks,x_ic=0.1,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 29.5, r_obs = 39.4)
  variable.names.Seasonal_RandomWalk_Obs_error <- c("tau_proc", "tau_obs")
  variable.namesout.Seasonal_RandomWalk_Obs_error <- c("tau_proc","tau_obs","mu")
  init.Seasonal_RandomWalk_Obs_error <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.Seasonal_RandomWalk_Obs_error <- c("tau_proc","tau_obs")
  
#Seasonal_Temperature_Obs_error
  data.Seasonal_Temperature_Obs_error <- list(y=y, year_no = year_no, beta.m1=0, beta.m2=0, beta.m3=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=0.1,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.001, r_obs = 0.001)
  variable.names.Seasonal_Temperature_Obs_error <- c("tau_proc", "beta1","beta2","beta3", "tau_yr","tau_obs")
  variable.namesout.Seasonal_Temperature_Obs_error <- c("tau_proc", "beta1", "beta2", "beta3", "mu", "tau_yr", "yr","tau_obs")
  init.Seasonal_Temperature_Obs_error <- list(list(tau_proc=0.001, tau_yr=0.001, tau_obs = 0.1, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1, tau_yr=0.1, tau_obs = 1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_yr=1, tau_obs = 5, beta1=0.5,beta2=0.5,beta3=0.5))
  params.Seasonal_Temperature_Obs_error <- c("tau_proc","beta1", "beta2", "beta3", "tau_yr","tau_obs")
  
  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  init = eval(parse(text = paste0('init.', model_name)))
  params = eval(parse(text = paste0('params.', model_name)))

  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params)) 
}





preds_plug_ins <- function(model_name){
  
## One step ahead prediction intervals
dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
    filter(site == "midge")
  
times <- as.Date(as.character(dat$date))
  
samp <- sample.int(nrow(out),nsamp)
mus=c(grep("mu\\[1,", colnames(out)),grep("mu\\[2,", colnames(out)),
      grep("mu\\[3,", colnames(out)),grep("mu\\[4,", colnames(out)),
      grep("mu\\[5,", colnames(out)),grep("mu\\[6,", colnames(out)))
mu = out[samp,mus] 
Temps=c(Temp[1,], Temp[2,], Temp[3,], Temp[4,], Temp[5,], Temp[6,])

samp <- sample.int(nrow(out),nsamp)
mus=grep("mu", colnames(out))
mu = out[samp,mus] 
times=c(1:length(mus))
  
#Seasonal_RandomWalk_Obs_error

if(model_name=="Seasonal_RandomWalk_Obs_error"){
tau_add = out[samp,grep("tau_add",colnames(out))]
tau_obs = out[samp,grep("tau_obs",colnames(out))]
pred.Seasonal_RandomWalk_Obs_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.Seasonal_RandomWalk_Obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
year_no <- c(1:6)
season_weeks <- c(1:20)
mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)

for(k in 1:max(year_no)){
  
  mydata <- mu[,grep(mu_greps[k],colnames(mu))]
  
  t <- ts[k,]
  
  for(j in 2:max(season_weeks)){
    
    #process model
    pred.Seasonal_RandomWalk_Obs_error[,t[j]] = rtruncnorm(nsamp,a = 0, b = Inf, mean = mydata[,j-1],sd = 1/tau_proc)
    
    #data model
    pred_obs.Seasonal_RandomWalk_Obs_error[,t[j]] = rnorm(nsamp,pred.Seasonal_RandomWalk_Obs_error[,t[j]],1/tau_obs)
    
  }
 }
}

#RandomYearIntercept

if(model_name=="RandomYearIntercept"){
tau_add = out[samp,grep("tau_add",colnames(out))]
yr_temp = out[samp,grep("yr",colnames(out))]
yr=yr_temp[,-1]
beta = out[samp,grep("beta",colnames(out))]

pred.RandomYearIntercept <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomYearIntercept <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
x<- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  x[,t] <- beta[t] + mu[,t-1] + yr[,year_no[t]]
  pred.RandomYearIntercept[,t] = rnorm(nsamp, x[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.RandomYearIntercept[,t]) 
  pred_obs.RandomYearIntercept[,t] = rpois(nsamp, m)}
}

#Temperature_obs_error

if(model_name=="Temperature_obs_error"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  sd_obs = out[samp,grep("sd_obs",colnames(out))]
  yr_temp = out[samp,grep("yr",colnames(out))]
  yr=yr_temp[,-1]
  
  pred.Temperature_obs_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Temperature_obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  
  for (t in 2:ncol(mu)){
    lambda[,t] <- beta1[t] + beta2[t]*mu[,t-1] + beta3[t]*Temp[t] + yr[,year_no[t]]
    pred.Temperature_obs_error[,t] = rnorm(nsamp, lambda[,t], tau_proc) 
    m <- pred.Temperature_obs_error[,t] 
    pred_obs.Temperature_obs_error[,t] = rnorm(nsamp, m, 1/sd_obs^2)}
}



pred_obs= eval(parse(text = paste0('pred_obs.', model_name)))
pred = eval(parse(text = paste0('pred.', model_name)))

return(list(pred_obs.model = pred_obs, pred.model = pred)) 

}
