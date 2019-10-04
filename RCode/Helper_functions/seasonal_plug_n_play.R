# plug and play scripts 
# JAZ 2019-02-15
# WB Updates
# MEL updates for seasonal for-loop 30JUL19

jags_plug_ins <- function(model_name){

#JAGS Plug-ins: Add each separate model here 
#variable.names are variables you would like to plot for model convergence (e.g., excludes mu)
#variable.names.out are all variables you would like to monitor in the jags run 
#init are a range of initial conditions for parameters in each of 3 chains 

#Seasonal_RandomWalk
  data.Seasonal_RandomWalk <- list(y=y, year_no = year_no,season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.001, r_obs = 0.001)
  variable.names.Seasonal_RandomWalk <- c("tau_proc", "tau_obs")
  variable.namesout.Seasonal_RandomWalk <- c("tau_proc","tau_obs","mu")
  init.Seasonal_RandomWalk <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.Seasonal_RandomWalk <- c("tau_proc","tau_obs")
  

#Seasonal_RandomWalk_Obs_error 
  data.Seasonal_RandomWalk_Obs_error <- list(y=y, year_no = year_no,season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_RandomWalk_Obs_error <- c("tau_proc", "tau_obs")
  variable.namesout.Seasonal_RandomWalk_Obs_error <- c("tau_proc","tau_obs","mu")
  init.Seasonal_RandomWalk_Obs_error <- list(list(tau_proc=0.001, tau_obs = 0.1), list(tau_proc=0.1, tau_obs = 1), list(tau_proc=1, tau_obs = 5))
  params.Seasonal_RandomWalk_Obs_error <- c("tau_proc","tau_obs")
  
#Seasonal_RandomWalk_RandomYear 
  data.Seasonal_RandomWalk_RandomYear <- list(y=y, year_no = year_no,season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_RandomWalk_RandomYear <- c("tau_proc", "tau_obs","tau_yr")
  variable.namesout.Seasonal_RandomWalk_RandomYear <- c("tau_proc","tau_obs","mu","tau_yr","yr")
  init.Seasonal_RandomWalk_RandomYear <- list(list(tau_proc=0.001, tau_obs = 0.1, tau_yr = 0.001), list(tau_proc=0.1, tau_obs = 1, tau_yr = 0.1), list(tau_proc=1, tau_obs = 5, tau_yr = 1))
  params.Seasonal_RandomWalk_RandomYear <- c("tau_proc","tau_obs","tau_yr")

#Seasonal_AR
  data.Seasonal_AR <- list(y=y, year_no = year_no,beta.m1=0,  beta.m2=0,beta.v1=0.001, beta.v2=0.001,  season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_AR <- c("tau_proc", "beta1","beta2", "tau_obs")
  variable.namesout.Seasonal_AR <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs")
  init.Seasonal_AR <- list(list(tau_proc=0.001, tau_obs = 0.1,  beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5, beta1=0.5,beta2=0.5))
  params.Seasonal_AR <- c("tau_proc","beta1", "beta2",  "tau_obs")
  
  
#Seasonal_Temperature_Obs_error
  data.Seasonal_Temperature_Obs_error <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0, beta.v1=0.001, beta.v2=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84) #x_T_ic = 14, tau_T_ic = 100)
  variable.names.Seasonal_Temperature_Obs_error <- c("tau_proc", "beta1","beta2",  "tau_obs","tau_T_proc")
  variable.namesout.Seasonal_Temperature_Obs_error <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs", "tau_T_proc")
  init.Seasonal_Temperature_Obs_error <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01, beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1, beta1=0.5,beta2=0.5))
  params.Seasonal_Temperature_Obs_error <- c("tau_proc","beta1", "beta2","tau_obs","tau_T_proc")
  
#Seasonal_AR_Temperature
  data.Seasonal_AR_Temperature <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_AR_Temperature <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_T_proc")
  variable.namesout.Seasonal_AR_Temperature <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_T_proc")
  init.Seasonal_AR_Temperature <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_T_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_T_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Seasonal_AR_Temperature <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_T_proc")

#Seasonal_AR_Mintemp
  data.Seasonal_AR_Mintemp <- list(y=y, year_no = year_no,week_min = week_min, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84, x_T_ic = 14, tau_T_ic = 100)
  variable.names.Seasonal_AR_Mintemp <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_T_obs","tau_T_proc")
  variable.namesout.Seasonal_AR_Mintemp <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs","tau_T_obs","mu_T", "tau_T_proc")
  init.Seasonal_AR_Mintemp <- list(list(tau_proc=0.001, tau_obs = 0.1, tau_T_obs = 0.01, tau_T_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_obs = 0.1,tau_T_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_T_obs = 1,tau_T_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Seasonal_AR_Mintemp <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_T_obs","tau_T_proc")

#Seasonal_AR_Mintemp_Lag
  data.Seasonal_AR_Mintemp_Lag <- list(y=y, year_no = year_no,week_min = week_min, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84, x_T_ic = 14, tau_T_ic = 100)
  variable.names.Seasonal_AR_Mintemp_Lag <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_T_obs","tau_T_proc")
  variable.namesout.Seasonal_AR_Mintemp_Lag <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs","tau_T_obs","mu_T", "tau_T_proc")
  init.Seasonal_AR_Mintemp_Lag <- list(list(tau_proc=0.001, tau_obs = 0.1, tau_T_obs = 0.01, tau_T_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_T_obs = 0.1,tau_T_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_T_obs = 1,tau_T_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Seasonal_AR_Mintemp_Lag <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_T_obs","tau_T_proc")
  
#Seasonal_Temperature_RandomYear_Obs_error
  data.Seasonal_Temperature_RandomYear_Obs_error <- list(y=y, year_no = year_no, beta.m1=0, beta.m2=0, beta.m3=0,beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=0.1,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.001, r_obs = 0.001, T_mean = 21.04)
  variable.names.Seasonal_Temperature_RandomYear_Obs_error <- c("tau_proc", "beta1","beta2","beta3", "tau_yr","tau_obs","tau_T_obs")
  variable.namesout.Seasonal_Temperature_RandomYear_Obs_error <- c("tau_proc", "beta1", "beta2", "beta3", "mu", "tau_yr", "yr","tau_obs","tau_T_obs","Temp")
  init.Seasonal_Temperature_RandomYear_Obs_error <- list(list(tau_proc=0.001, tau_yr=0.001, tau_obs = 0.1, tau_T_obs = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1, tau_yr=0.1, tau_obs = 1,tau_T_obs = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_yr=1, tau_obs = 5,tau_T_obs = 1, beta1=0.5,beta2=0.5,beta3=0.5))
  params.Seasonal_Temperature_RandomYear_Obs_error <- c("tau_proc","beta1", "beta2", "beta3", "tau_yr","tau_obs","tau_T_obs")

#Seasonal_Airtemp_Obs_error
  data.Seasonal_Airtemp_Obs_error <- list(y=y, year_no = year_no, beta.m1=0, beta.m2=0, beta.v1=0.001, beta.v2=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_Airtemp_Obs_error <- c("tau_proc", "beta1","beta2","tau_obs")
  variable.namesout.Seasonal_Airtemp_Obs_error <- c("tau_proc", "beta1", "beta2",  "mu","tau_obs")
  init.Seasonal_Airtemp_Obs_error <- list(list(tau_proc=0.001,  tau_obs = 0.1,  beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0), list(tau_proc=1,  tau_obs = 5, beta1=0.5,beta2=0.5))
  params.Seasonal_Airtemp_Obs_error <- c("tau_proc","beta1", "beta2", "tau_obs")

#Seasonal_AR_Airtemp
  data.Seasonal_AR_Airtemp <- list(y=y, year_no = year_no, beta.m1=0, beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Temp=Temp, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_AR_Airtemp <- c("tau_proc", "beta1","beta2","beta3","tau_obs")
  variable.namesout.Seasonal_AR_Airtemp <- c("tau_proc", "beta1", "beta2","beta3",  "mu","tau_obs")
  init.Seasonal_AR_Airtemp <- list(list(tau_proc=0.001,  tau_obs = 0.1,  beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1, beta1=0, beta2=0, beta3=0), list(tau_proc=1,  tau_obs = 5, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Seasonal_AR_Airtemp <- c("tau_proc","beta1", "beta2","beta3", "tau_obs")
  
#Seasonal_Schmidt_Obs_error
  data.Seasonal_Schmidt_Obs_error <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0, beta.v1=0.001, beta.v2=0.001, Schmidt=Schmidt, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_Schmidt_Obs_error <- c("tau_proc", "beta1","beta2",  "tau_obs","tau_S_proc")
  variable.namesout.Seasonal_Schmidt_Obs_error <- c("tau_proc", "beta1", "beta2",  "mu", "tau_obs", "tau_S_proc")
  init.Seasonal_Schmidt_Obs_error <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_S_proc = 0.1, beta1=0, beta2=0), list(tau_proc=1, tau_obs = 5,tau_S_proc = 1, beta1=0.5,beta2=0.5))
  params.Seasonal_Schmidt_Obs_error <- c("tau_proc","beta1", "beta2","tau_obs","tau_S_proc")

#Seasonal_AR_Schmidt
  data.Seasonal_AR_Schmidt <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Schmidt=Schmidt, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84)
  variable.names.Seasonal_AR_Schmidt <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_S_proc")
  variable.namesout.Seasonal_AR_Schmidt <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs", "tau_S_proc")
  init.Seasonal_AR_Schmidt <- list(list(tau_proc=0.001, tau_obs = 0.1,  tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Seasonal_AR_Schmidt <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_S_proc")
  
#Seasonal_AR_MaxSchmidt_Lag
  data.Seasonal_AR_MaxSchmidt_Lag <- list(y=y, year_no = year_no,week_avg = week_avg, beta.m1=0,  beta.m2=0,beta.m3=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, Schmidt=Schmidt, season_weeks=season_weeks,x_ic=-5,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 15.37, r_obs = 7.84, x_S_ic = 177, tau_S_ic = 100)
  variable.names.Seasonal_AR_MaxSchmidt_Lag <- c("tau_proc", "beta1","beta2", "beta3", "tau_obs","tau_S_obs","tau_S_proc")
  variable.namesout.Seasonal_AR_MaxSchmidt_Lag <- c("tau_proc", "beta1", "beta2","beta3",  "mu", "tau_obs","tau_S_obs","mu_S", "tau_S_proc")
  init.Seasonal_AR_MaxSchmidt_Lag <- list(list(tau_proc=0.001, tau_obs = 0.1, tau_S_obs = 0.01, tau_S_proc = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5), list(tau_proc=0.1,  tau_obs = 1,tau_S_obs = 0.1,tau_S_proc = 0.1, beta1=0, beta2=0, beta3=0), list(tau_proc=1, tau_obs = 5,tau_S_obs = 1,tau_S_proc = 1, beta1=0.5,beta2=0.5, beta3=0.5))
  params.Seasonal_AR_MaxSchmidt_Lag <- c("tau_proc","beta1", "beta2", "beta3","tau_obs","tau_S_obs","tau_S_proc")
  
#Seasonal_TempQuad_Obs_error
  data.Seasonal_TempQuad_Obs_error <- list(y=y, year_no = year_no, beta.m1=0, beta.m2=0, beta.m3=0,beta.m4=0, beta.v1=0.001, beta.v2=0.001,beta.v3=0.001, beta.v4=0.001,Temp=Temp, season_weeks=season_weeks,x_ic=0.1,tau_ic = 100,a_proc = 0.001,r_proc = 0.001, a_obs = 0.001, r_obs = 0.001, T_mean = 21.04, tau_proc_T = 0.1)
  variable.names.Seasonal_TempQuad_Obs_error <- c("tau_proc", "beta1","beta2","beta3","beta4", "tau_yr","tau_obs","tau_T_obs")
  variable.namesout.Seasonal_TempQuad_Obs_error <- c("tau_proc", "beta1", "beta2", "beta3","beta4", "mu", "tau_yr", "yr","tau_obs","tau_T_obs","mu_T")
  init.Seasonal_TempQuad_Obs_error <- list(list(tau_proc=0.001, tau_yr=0.001, tau_obs = 0.1, tau_T_obs = 0.01, beta1=-0.5, beta2=-0.5, beta3=-0.5, beta4=-0.5), list(tau_proc=0.1, tau_yr=0.1, tau_obs = 1,tau_T_obs = 0.1, beta1=0, beta2=0, beta3=0, beta4=0), list(tau_proc=1, tau_yr=1, tau_obs = 5,tau_T_obs = 1, beta1=0.5,beta2=0.5,beta3=0.5, beta4=0.5))
  params.Seasonal_TempQuad_Obs_error <- c("tau_proc","beta1", "beta2", "beta3","beta4", "tau_yr","tau_obs","tau_T_obs")
  
  
  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  init = eval(parse(text = paste0('init.', model_name)))
  params = eval(parse(text = paste0('params.', model_name)))

  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init, params.model = params)) 
}





preds_plug_ins <- function(model_name){
  
## One step ahead prediction intervals
nsamp = 1500
dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
    filter(site == "midge")
  
#times <- as.Date(as.character(dat$date))
  
samp <- sample.int(nrow(out),nsamp)

mus=c(grep("mu\\[1,", colnames(out)),grep("mu\\[2,", colnames(out)),
      grep("mu\\[3,", colnames(out)),grep("mu\\[4,", colnames(out)),
      grep("mu\\[5,", colnames(out)),grep("mu\\[6,", colnames(out)))
mu = out[samp,mus] 

mu_Ts=c(grep("mu_T\\[1,", colnames(out)),grep("mu_T\\[2,", colnames(out)),
      grep("mu_T\\[3,", colnames(out)),grep("mu_T\\[4,", colnames(out)),
      grep("mu_T\\[5,", colnames(out)),grep("mu_T\\[6,", colnames(out)))
mu_T = out[samp,mu_Ts] 

mus_S=c(grep("mu_S\\[1,", colnames(out)),grep("mu_S\\[2,", colnames(out)),
        grep("mu_S\\[3,", colnames(out)),grep("mu_S\\[4,", colnames(out)),
        grep("mu_S\\[5,", colnames(out)),grep("mu_S\\[6,", colnames(out)))
mu_S = out[samp,mus_S] 

Temps=c(Temp[1,], Temp[2,], Temp[3,], Temp[4,], Temp[5,], Temp[6,])
Schmidts=c(Schmidt[1,], Schmidt[2,], Schmidt[3,], Schmidt[4,], Schmidt[5,], Schmidt[6,])
week_avg = week_avg
# samp <- sample.int(nrow(out),nsamp)
# mus=grep("mu", colnames(out))
# mu = out[samp,mus] 
# times=c(1:length(mus))

#Seasonal_RandomWalk

if(model_name=="Seasonal_RandomWalk"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  pred.Seasonal_RandomWalk <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_RandomWalk <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      pred.Seasonal_RandomWalk[,t[j]] = rnorm(nsamp,mydata[,j-1],tau_proc)
      
      #data model
      pred_obs.Seasonal_RandomWalk[,t[j]] = rnorm(nsamp,pred.Seasonal_RandomWalk[,t[j]],tau_obs)
      
    }
  }
}

  
#Seasonal_RandomWalk_Obs_error

if(model_name=="Seasonal_RandomWalk_Obs_error"){
tau_proc = out[samp,grep("tau_proc",colnames(out))]
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
    pred.Seasonal_RandomWalk_Obs_error[,t[j]] = rnorm(nsamp,mydata[,j-1],tau_proc)
    
    #data model
    pred_obs.Seasonal_RandomWalk_Obs_error[,t[j]] = rnorm(nsamp,pred.Seasonal_RandomWalk_Obs_error[,t[j]],tau_obs)
    
  }
 }
}

#Seasonal_RandomWalk_RandomYear

if(model_name=="Seasonal_RandomWalk_RandomYear"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_yr = out[samp,grep("tau_yr",colnames(out))]
  pred.Seasonal_RandomWalk_RandomYear <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_RandomWalk_RandomYear <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  x<- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  yr_temp = out[samp,grep("yr",colnames(out))]
  yr=yr_temp[,-1]
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      x[,j] <- mydata[,j-1] + yr[,year_no[k]]
      pred.Seasonal_RandomWalk_RandomYear[,t[j]] = rnorm(nsamp,x[,j],tau_proc)
      
      #data model
      pred_obs.Seasonal_RandomWalk_RandomYear[,t[j]] = rnorm(nsamp,pred.Seasonal_RandomWalk_RandomYear[,t[j]],tau_obs)
      
    }
  }
}

#Seasonal_AR
if(model_name=="Seasonal_AR"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  pred.Seasonal_AR <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      lambda[,t[j]] <- beta1 + beta2*mydata[,j-1] 
      pred.Seasonal_AR[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR[,t[j]] = rnorm(nsamp,pred.Seasonal_AR[,t[j]],tau_obs)
    }
  }
}


#Seasonal_Temperature_Obs_error
if(model_name=="Seasonal_Temperature_Obs_error"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_T_proc = out[samp,grep("tau_T_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  pred.Seasonal_Temperature_Obs_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_Temperature_Obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Tempz = Temp
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    #myTempdata <- mu_T[,grep(mu_T_greps[k],colnames(mu_T))]

    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Tempz[k,j])){lambda[,t[j]] <- beta1 + beta2*rnorm(nsamp,week_avg[j],tau_T_proc)}
      else{lambda[,t[j]] <- beta1 + beta2*Tempz[k,j] }
      
      pred.Seasonal_Temperature_Obs_error[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_Temperature_Obs_error[,t[j]] = rnorm(nsamp,pred.Seasonal_Temperature_Obs_error[,t[j]],tau_obs)
    }
  }
}

#Seasonal_AR_Temperature
if(model_name=="Seasonal_AR_Temperature"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_T_proc = out[samp,grep("tau_T_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  pred.Seasonal_AR_Temperature <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR_Temperature <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Tempz = Temp
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]

    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Tempz[k,j])){lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*rnorm(nsamp,week_avg[j],tau_T_proc)}
      else{lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*Tempz[k,j] }
      
      pred.Seasonal_AR_Temperature[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR_Temperature[,t[j]] = rnorm(nsamp,pred.Seasonal_AR_Temperature[,t[j]],tau_obs)
    }
  }
}

#Seasonal_AR_Mintemp
if(model_name=="Seasonal_AR_Mintemp"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_T_obs = out[samp,grep("tau_T_obs",colnames(out))]
  tau_T_proc = out[samp,grep("tau_T_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  pred.Seasonal_AR_Mintemp <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR_Mintemp <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  mu_T_greps <- c("mu_T\\[1,","mu_T\\[2,","mu_T\\[3,","mu_T\\[4,","mu_T\\[5,","mu_T\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Tempz = Temp
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    myTempdata <- mu_T[,grep(mu_T_greps[k],colnames(mu_T))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Tempz[k,j])){lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*myTempdata[t[j]]}
      else{lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*Tempz[k,j] }
      
      pred.Seasonal_AR_Mintemp[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR_Mintemp[,t[j]] = rnorm(nsamp,pred.Seasonal_AR_Mintemp[,t[j]],tau_obs)
    }
  }
}

#Seasonal_AR_Mintemp_Lag
if(model_name=="Seasonal_AR_Mintemp_Lag"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_T_obs = out[samp,grep("tau_T_obs",colnames(out))]
  tau_T_proc = out[samp,grep("tau_T_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  pred.Seasonal_AR_Mintemp_Lag <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR_Mintemp_Lag <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  mu_T_greps <- c("mu_T\\[1,","mu_T\\[2,","mu_T\\[3,","mu_T\\[4,","mu_T\\[5,","mu_T\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Tempz = Temp
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    myTempdata <- mu_T[,grep(mu_T_greps[k],colnames(mu_T))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Tempz[k,j-1])){lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*myTempdata[t[j-1]]}
      else{lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*Tempz[k,j-1] }
      
      pred.Seasonal_AR_Mintemp_Lag[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR_Mintemp_Lag[,t[j]] = rnorm(nsamp,pred.Seasonal_AR_Mintemp_Lag[,t[j]],tau_obs)
    }
  }
}

#Seasonal_Schmidt_Obs_error
if(model_name=="Seasonal_Schmidt_Obs_error"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_S_proc = out[samp,grep("tau_S_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  pred.Seasonal_Schmidt_Obs_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_Schmidt_Obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Schmidtz = Schmidt
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]

    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Schmidtz[k,j])){lambda[,t[j]] <- beta1 + beta2*rnorm(nsamp,week_avg[j],tau_S_proc)}
      else{lambda[,t[j]] <- beta1 + beta2*Schmidtz[k,j] }
      
      pred.Seasonal_Schmidt_Obs_error[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_Schmidt_Obs_error[,t[j]] = rnorm(nsamp,pred.Seasonal_Schmidt_Obs_error[,t[j]],tau_obs)
    }
  }
}

#Seasonal_AR_Schmidt
if(model_name=="Seasonal_AR_Schmidt"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_S_proc = out[samp,grep("tau_S_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  pred.Seasonal_AR_Schmidt <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR_Schmidt <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Schmidtz = Schmidt
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]

    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Schmidtz[k,j])){lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*rnorm(nsamp,week_avg[j],tau_S_proc)}
      else{lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*Schmidtz[k,j] }
      
      pred.Seasonal_AR_Schmidt[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR_Schmidt[,t[j]] = rnorm(nsamp,pred.Seasonal_AR_Schmidt[,t[j]],tau_obs)
    }
  }
}

#Seasonal_AR_MaxSchmidt_Lag
if(model_name=="Seasonal_AR_MaxSchmidt_Lag"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  tau_S_obs = out[samp,grep("tau_S_obs",colnames(out))]
  tau_S_proc = out[samp,grep("tau_S_proc",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  pred.Seasonal_AR_MaxSchmidt_Lag <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR_MaxSchmidt_Lag <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  mu_S_greps <- c("mu_S\\[1,","mu_S\\[2,","mu_S\\[3,","mu_S\\[4,","mu_S\\[5,","mu_S\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  Schmidtz = Schmidt
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    mySchmidtdata <- mu_S[,grep(mu_S_greps[k],colnames(mu_S))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      #filling Temp NAs
      if(is.na(Schmidtz[k,j-1])){lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*mySchmidtdata[t[j-1]]}
      else{lambda[,t[j]] <- beta1 + beta2*mydata[,j-1]+ beta3*Schmidtz[k,j-1] }
      
      pred.Seasonal_AR_MaxSchmidt_Lag[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR_MaxSchmidt_Lag[,t[j]] = rnorm(nsamp,pred.Seasonal_AR_MaxSchmidt_Lag[,t[j]],tau_obs)
    }
  }
}

#Seasonal_Airtemp_Obs_error
if(model_name=="Seasonal_Airtemp_Obs_error"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  pred.Seasonal_Airtemp_Obs_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_Airtemp_Obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]

    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      lambda[,t[j]] <- beta1 + beta2*Temp[k,j]
      pred.Seasonal_Airtemp_Obs_error[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_Airtemp_Obs_error[,t[j]] = rnorm(nsamp,pred.Seasonal_Airtemp_Obs_error[,t[j]],tau_obs)
    }
  }
}

#Seasonal_AR_Airtemp
if(model_name=="Seasonal_AR_Airtemp"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  pred.Seasonal_AR_Airtemp <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_AR_Airtemp <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      lambda[,t[j]] <- beta1 + beta2*mydata[,j-1] + beta3*Temp[k,j]
      pred.Seasonal_AR_Airtemp[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_AR_Airtemp[,t[j]] = rnorm(nsamp,pred.Seasonal_AR_Airtemp[,t[j]],tau_obs)
    }
  }
}


#Seasonal_Temperature_RandomYear_Obs_error

if(model_name=="Seasonal_Temperature_RandomYear_Obs_error"){
  tau_proc = out[samp,grep("tau_proc",colnames(out))]
  tau_obs = out[samp,grep("tau_obs",colnames(out))]
  beta1 = out[samp,grep("beta1",colnames(out))]
  beta2 = out[samp,grep("beta2",colnames(out))]
  beta3 = out[samp,grep("beta3",colnames(out))]
  tau_yr = out[samp,grep("tau_yr",colnames(out))]
  tau_T_obs = out[samp,grep("tau_T_obs",colnames(out))]
  yr_temp = out[samp,grep("yr",colnames(out))]
  yr=yr_temp[,-1]
  pred.Seasonal_Temperature_RandomYear_Obs_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
  pred_obs.Seasonal_Temperature_RandomYear_Obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  predT.Seasonal_Temperature_Obs_RandomYear_error <- matrix(NA,nrow=nsamp,ncol=ncol(mu_T))
  predT_obs.Seasonal_Temperature_RandomYear_Obs_error <- matrix(NA, nrow=nsamp, ncol=ncol(mu_T))
  year_no <- c(1:6)
  season_weeks <- c(1:20)
  mu_greps <- c("mu\\[1,","mu\\[2,","mu\\[3,","mu\\[4,","mu\\[5,","mu\\[6,")
  mu_T_greps <- c("mu_T\\[1,","mu_T\\[2,","mu_T\\[3,","mu_T\\[4,","mu_T\\[5,","mu_T\\[6,")
  ts = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
  
  for(k in 1:max(year_no)){
    
    mydata <- mu[,grep(mu_greps[k],colnames(mu))]
    myTempdata <- mu_T[,grep(mu_T_greps[k],colnames(mu_T))]
    
    t <- ts[k,]
    
    for(j in 2:max(season_weeks)){
      
      #process model
      
      #temperature
      predT.Seasonal_Temperature_RandomYear_Obs_error[,t[j]] = rnorm(nsamp, 21.04,0.1)
      if(is.na(Temps[t[j]])){Temps[t[j]]<- mean(predT.Seasonal_Temperature_RandomYear_Obs_error[,t[j]])}

      #Gloeo
      lambda[,t[j]] <- beta1 + beta2*mydata[,j-1] + beta3*Temps[t[j]] + yr[,year_no[k]]
      pred.Seasonal_Temperature_RandomYear_Obs_error[,t[j]] = rnorm(nsamp,lambda[,t[j]],tau_proc)
      
      #data model
      pred_obs.Seasonal_Temperature_RandomYear_Obs_error[,t[j]] = rnorm(nsamp,pred.Seasonal_Temperature_RandomYear_Obs_error[,t[j]],tau_obs)
      predT_obs.Seasonal_Temperature_RandomYear_Obs_error[,t[j]] = rnorm(nsamp,predT.Seasonal_Temperature_RandomYear_Obs_error[,t[j]],tau_T_obs)
  
    }
  }
}




pred_obs= eval(parse(text = paste0('pred_obs.', model_name)))
pred = eval(parse(text = paste0('pred.', model_name)))

return(list(pred_obs.model = pred_obs, pred.model = pred)) 

}
