model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    #y[i] ~ dlnorm(mu[i],1/sd_proc^2)
    y[i] ~ dnorm(mu[i],1/sd_obs^2)
    #m[i] <- exp(mu[i])
    
  }
  
  #### Process Model
  
  for(i in 2:N){
    mu[i]~dnorm(lambda[i],tau_proc) #mu's here are on log scale
    lambda[i] <- beta1 + beta2*mu[i-1] + beta3*Temp[i] + yr[year_no[i]]
  }
  
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_proc ~ dgamma(a_add,r_add)
  beta1 ~ dnorm(beta.m1,beta.v1) 
  beta2 ~ dnorm(beta.m2,beta.v2) 
  beta3 ~ dnorm(beta.m3,beta.v3) 
  tau_yr ~ dgamma(0.01,0.01)
  lambda[1] ~ dnorm(x_ic, tau_ic)
  sd_obs ~ dnorm(155,1/908^2)
  
  #Loops through number of years and defines prior for each year 
  for(k in 1:max(year_no)) {
    yr[k] ~ dnorm(0,tau_yr)
  }
  
}