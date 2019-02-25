## One step ahead prediction intervals

#RandomWalk

tau_add = out[samp,grep("tau_add",colnames(out))]
pred.RandomWalk <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomWalk <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  pred.RandomWalk[,t] = rnorm(nsamp,mu[,t-1],tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.RandomWalk[,t]) 
  pred_obs.RandomWalk[,t] = rpois(nsamp, m)}

#RandomWalkZip


tau_add = out[samp,grep("tau_add",colnames(out))]
theta = out[samp,grep("theta",colnames(out))]
pred.RandomWalkZip <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomWalkZip <- matrix(NA, nrow=nsamp, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  pred.RandomWalkZip[,t] = rnorm(nsamp,mu[,t-1],tau_add) #exponentiate these before comparing to data, because mu on log scale
  b <- rbinom(nsamp, 1, theta)
  m <- exp(pred.RandomWalkZip[,t])*b+ 1E-10 
  pred_obs.RandomWalkZip[,t] = rpois(nsamp, m)}

#RandomYear 

tau_add = out[samp,grep("tau_add",colnames(out))]
yr_temp = out[samp,grep("yr",colnames(out))]
yr=yr_temp[,2:9]
#x = out[samp,grep("x",colnames(out))]

pred.RandomYear <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomYear <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
x<- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  x[t] <- mu[,t-1] + yr[year_no[t]]
  pred.RandomYear[,t] = rnorm(nsamp, x[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.RandomYear[,t]) 
  pred_obs.RandomYear[,t] = rpois(nsamp, m)}

#RandomYearIntercept

tau_add = out[samp,grep("tau_add",colnames(out))]
yr_temp = out[samp,grep("yr",colnames(out))]
yr=yr_temp[,2:9]
#x = out[samp,grep("x",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.RandomYearIntercept <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.RandomYearIntercept <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
x<- matrix(NA, nrow=1, ncol=ncol(mu))
for (t in 2:ncol(mu)){
  x[t] <- beta[t] + mu[,t-1] + yr[year_no[t]]
  pred.RandomYearIntercept[,t] = rnorm(nsamp, x[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.RandomYearIntercept[,t]) 
  pred_obs.RandomYearIntercept[,t] = rpois(nsamp, m)}

#Exponential

tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.Exponential <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.Exponential <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[t] <- beta[t]*mu[,t-1] 
  pred.Exponential[,t] = rnorm(nsamp, lambda[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.Exponential[,t]) 
  pred_obs.Exponential[,t] = rpois(nsamp, m)}

#Logistic

tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.Logistic <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.Logistic <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[t] <- beta[t,1]*mu[,t-1] + beta[t,2]*mu[,t-1]*mu[,t-1]
  pred.Logistic[,t] = rnorm(nsamp, lambda[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.Logistic[,t]) 
  pred_obs.Logistic[,t] = rpois(nsamp, m)}

#DayLength

tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.DayLength <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.DayLength <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*DL[t]
  pred.DayLength[,t] = rnorm(nsamp, lambda[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred[,t]) 
  pred_obs.DayLength[,t] = rpois(nsamp, m)}

#DayLengthQuad

tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.DayLengthQuad <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.DayLengthQuad <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*DL[t] + beta[t,4]*DL[t]*DL[t]
  pred.DayLengthQuad[,t] = rnorm(nsamp, lambda[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.DayLengthQuad[,t]) 
  pred_obs.DayLengthQuad[,t] = rpois(nsamp, m)}

#TempExp

tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.TempExp <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.TempExp <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*Temp[t]
  pred.TempExp[,t] = rnorm(nsamp, lambda[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred[,t]) 
  pred_obs.TempExp[,t] = rpois(nsamp, m)}

#TempQuad

tau_add = out[samp,grep("tau_add",colnames(out))]
beta = out[samp,grep("beta",colnames(out))]

pred.TempQuad <- matrix(NA,nrow=nsamp,ncol=ncol(mu))
pred_obs.TempQuad <- matrix(NA, nrow=nsamp, ncol=ncol(mu))
lambda <- matrix(NA, nrow=1, ncol=ncol(mu))

for (t in 2:ncol(mu)){
  lambda[t] <- beta[t,1] + beta[t,2]*mu[,t-1] + beta[t,3]*DL[t] + beta[t,4]*Temp[t]*Temp[t]
  pred.TempQuad[,t] = rnorm(nsamp, lambda[t], tau_add) #exponentiate these before comparing to data, because mu on log scale
  m <- exp(pred.TempQuad[,t]) 
  pred_obs.TempQuad[,t] = rpois(nsamp, m)}

