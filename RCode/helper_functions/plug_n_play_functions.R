# plug and play scripts 
# JAZ 2019-02-15
# WB Updates


plug_n_play_data <- function(start_date, end_date, sites, model_timestep, fill_dates, forecast = FALSE, forecast_end_date){
  
  sites = tolower(site) 
  
  if(forecast){
    Data = get_data(cal_time_start = start_date, 
                    cal_time_end = end_date, 
                    model_timestep = model_timestep, # model timestep in days if filling in dates
                    fill_dates = fill_dates,  # T/F for filling in dates w/o observations with NA's 
                    forecast = forecast, 
                    forecast_time_end = forecast_end_date,
                    sites = sites)
    
    Data_cal = Data$cal %>% 
      mutate(daylength = daylength(43.4802, date))
    
    Data_forecast = Data$forecast %>%
      mutate(daylength = daylength(43.4802, date))
    
    return(list(cal = Data_cal, forecast = Data_forecast))
    
  }else{
    Data = get_data(cal_time_start = start_date, 
                    cal_time_end = end_date, 
                    model_timestep = model_timestep, # model timestep in days if filling in dates
                    fill_dates = fill_dates,  # T/F for filling in dates w/o observations with NA's 
                    sites = sites) %>%
      mutate(daylength = daylength(43.4802, date))
    
    return(Data) 
  }

  #2) return correct site - this is throwing an error - eliminating for now - MEL 10APR19
  #dat = eval(parse(text = sites))
}

jags_plug_ins <- function(model_name){

#JAGS Plug-ins: Add each separate model here 
#variable.names are variables you would like to plot for model convergence (e.g., excludes mu)
#variable.names.out are all variables you would like to monitor in the jags run 
#init are a range of initial conditions for parameters in each of 3 chains 
  
#Random Walk
  data.RandomWalk <- list(y=y, N=length(y),x_ic=log(0.1),tau_ic = 100, a_add = 0.001,r_add = 0.001)
  variable.names.RandomWalk<- c("tau_add")
  variable.namesout.RandomWalk<- c("tau_add", "mu")
  init.RandomWalk <- list(list(tau_add=0.001), list(tau_add=0.1), list(tau_add=1))
  
#RandomWalkZip
  data.RandomWalkZip <- list(y=y, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001, alpha=1, epsilon=1)
  variable.names.RandomWalkZip<- c("tau_add", "theta")
  variable.namesout.RandomWalkZip<- c("tau_add", "mu", "theta")
  init.RandomWalkZip <- list(list(tau_add=0.001, theta=0), list(tau_add=0.1, theta=0.5), list(tau_add=1, theta=1))
  
#DayLength  
  data.DayLength <- list(y=y, beta.m=as.vector(c(0,0,0)), beta.v=solve(diag(1E-03,3)), DL=DL, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.DayLength <- c("tau_add", "beta")
  variable.namesout.DayLength <- c("tau_add", "beta", "mu")
  init.DayLength <- list(list(tau_add=0.001, beta=c(-0.5,-0.5,-0.5)), list(tau_add=0.1, beta=c(0,0,0)), list(tau_add=1, beta=c(0.5,0.5,0.5)))
  
#DayLengthQuad
  data.DayLengthQuad <- list(y=y, beta.m=as.vector(c(0,0,0,0)), beta.v=solve(diag(1E-03,4)), DL=DL, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.DayLengthQuad <- c("tau_add", "beta")
  variable.namesout.DayLengthQuad <- c("tau_add", "beta", "mu")
  init.DayLengthQuad <- list(list(tau_add=0.001, beta=c(-0.5,-0.5,-0.5,-0.5)), list(tau_add=0.1, beta=c(0,0,0,0)), list(tau_add=1, beta=c(0.5,0.5,0.5,0.5)))
  
#TempExp
  data.TempExp <- list(y=y, beta.m=as.vector(c(0,0,0)), beta.v=solve(diag(1E-03,3)), Temp=Temp, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.TempExp <- c("tau_add", "beta")
  variable.namesout.TempExp <- c("tau_add", "beta", "mu")
  init.TempExp <- list(list(tau_add=0.001, beta=c(-0.5,-0.5,-0.5)), list(tau_add=0.1, beta=c(0,0,0)), list(tau_add=1, beta=c(0.5,0.5,0.5)))
  
#Temp_Quad
  data.Temp_Quad <- list(y=y, beta.m=as.vector(c(0,0,0,0)), beta.v=solve(diag(1E-03,4)), Temp=Temp, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.Temp_Quad <- c("tau_add", "beta")
  variable.namesout.Temp_Quad <- c("tau_add", "beta", "mu")
  init.Temp_Quad <- list(list(tau_add=0.001, beta=c(-0.5,-0.5,-0.5,-0.5)), list(tau_add=0.1, beta=c(0,0,0,0)), list(tau_add=1, beta=c(0.5,0.5,0.5,0.5)))
  
#Logistic
  data.Logistic <- list(y=y, beta.m=as.vector(c(0,0)), beta.v=solve(diag(1E-03,2)), N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.Logistic <- c("tau_add", "beta")
  variable.namesout.Logistic <- c("tau_add", "beta", "mu")
  init.Logistic <- list(list(tau_add=0.001, beta=c(-0.5,-0.5)), list(tau_add=0.1, beta=c(0,0)), list(tau_add=1, beta=c(0.5,0.5)))
  
#Exponential
  data.Exponential <- list(y=y, beta.m=0, beta.v=1E-03, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.Exponential <- c("tau_add", "beta")
  variable.namesout.Exponential <- c("tau_add", "beta", "mu")
  init.Exponential <- list(list(tau_add=0.001, beta=-0.5), list(tau_add=0.1, beta=0), list(tau_add=1, beta=0.5))
  
#RandomYear
  data.RandomYear <- list(y=y, year_no=year_no, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.RandomYear <- c("tau_add", "tau_yr")
  variable.namesout.RandomYear <- c("tau_add", "tau_yr","yr","mu", "x")
  init.RandomYear <- list(list(tau_add=0.001, tau_yr=0.001), list(tau_add=0.1, tau_yr=0.1), list(tau_add=1, tau_yr=1))
  
#RandomYearIntercept
  data.RandomYearIntercept <- list(y=y, year_no=year_no, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001, beta.m=0, beta.v=1E-03)
  variable.names.RandomYearIntercept <- c("tau_add", "tau_yr", "beta")
  variable.namesout.RandomYearIntercept <- c("tau_add", "tau_yr","yr","mu", "x", "beta")
  init.RandomYearIntercept <- list(list(tau_add=0.001, tau_yr=0.001, beta=-0.5), list(tau_add=0.1, tau_yr=0.1, beta=0), list(tau_add=1, tau_yr=1, beta=0.5))
  
#data.ChangepointTempExp <- list(y=y, beta.m=beta.m, beta.v=beta.v, Temp=Temp, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
# variable.names.ChangepointTempExp <- c("tau_add", "beta", "k")
# variable.namesout.ChangepointTempExp <- c("tau_add", "beta", "k", "mu")
  
  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  init = eval(parse(text = paste0('init.', model_name)))
  
  
  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout, init.model = init)) 
}

preds_plug_ins <- function(model_name){
  
## One step ahead prediction intervals

samp <- sample.int(nrow(out),nsamp)
mus=grep("mu", colnames(out))
mu = out[samp,mus] 
times=c(1:length(mus))
  
#RandomWalk

if(model_name=="RandomWalk"){
tau_add = out[samp,grep("tau_add",colnames(out))]
pred.RandomWalk <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomWalk <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  pred.RandomWalk[,t] = rnorm(nsamp,mu[,t-1],tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.RandomWalk[,t]) 
  pred_obs.RandomWalk[,t] = rpois(nsamp, m)}
}

#RandomWalkZip

if(model_name=="RandomWalkZip"){
tau_add = out[samp,grep("tau_add",colnames(out))]
theta = out[samp,grep("theta",colnames(out))]
pred.RandomWalkZip <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomWalkZip <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  pred.RandomWalkZip[,t] = rnorm(nsamp,mu[,t-1],tau_add) #exponentiate these before comparing to data, because mu on log scale
  b <- rbinom(nsamp, 1, theta)
  m <- exp(pred.RandomWalkZip[,t])*b+ 1E-10 
  pred_obs.RandomWalkZip[,t] = rpois(nsamp, m)}
}

#RandomYear 

if(model_name=="RandomYear"){
tau_add = out[samp,grep("tau_add",colnames(out))]
yr_temp = out[samp,grep("yr",colnames(out))]
yr=yr_temp[,-1]

pred.RandomYear <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomYear <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
x<- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  x[,t] <- mu[,t-1] + yr[,year_no[t]]
  pred.RandomYear[,t] = rnorm(nsamp, x[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.RandomYear[,t]) 
  pred_obs.RandomYear[,t] = rpois(nsamp, m)}
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

#Exponential

if(model_name=="Exponential"){
tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.Exponential <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.Exponential <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[,t] <- beta[,t]*mu[,t-1] 
  pred.Exponential[,t] = rnorm(nsamp, lambda[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.Exponential[,t]) 
  pred_obs.Exponential[,t] = rpois(nsamp, m)}
}

#Logistic

if(model_name=="Logistic"){
tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.Logistic <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.Logistic <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[,t] <- beta[,1]*mu[,t-1] + beta[,2]*mu[,t-1]*mu[,t-1]
  pred.Logistic[,t] = rnorm(nsamp, lambda[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.Logistic[,t]) 
  pred_obs.Logistic[,t] = rpois(nsamp, m)}
}

#DayLength

if(model_name=="DayLength"){
tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.DayLength <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.DayLength <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[,t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*DL[t]
  pred.DayLength[,t] = rnorm(nsamp, lambda[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred[,t]) 
  pred_obs.DayLength[,t] = rpois(nsamp, m)}
}

#DayLengthQuad

if(model_name =="DayLengthQuad"){
tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.DayLengthQuad <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.DayLengthQuad <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[,t] <- beta[,1] + beta[,2]*mu[,t-1] + beta[,3]*DL[t] + beta[,4]*DL[t]*DL[t]
  pred.DayLengthQuad[,t] = rnorm(nsamp, lambda[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.DayLengthQuad[,t]) 
  pred_obs.DayLengthQuad[,t] = rpois(nsamp, m)}
}

#TempExp

if(model=="TempExpt"){
tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.TempExp <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.TempExp <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[,t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*Temp[t]
  pred.TempExp[,t] = rnorm(nsamp, lambda[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred[,t]) 
  pred_obs.TempExp[,t] = rpois(nsamp, m)}
}

#TempQuad

if(model_name=="TempQuad"){
tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.TempQuad <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.TempQuad <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[,t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*DL[t] + beta[t,4]*Temp[t]*Temp[t]
  pred.TempQuad[,t] = rnorm(nsamp, lambda[,t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.TempQuad[,t]) 
  pred_obs.TempQuad[,t] = rpois(nsamp, m)}
}

pred_obs= eval(parse(text = paste0('pred_obs.', model_name)))
pred = eval(parse(text = paste0('pred.', model_name)))

return(list(pred_obs.model = pred_obs, pred.model = pred)) 

}
