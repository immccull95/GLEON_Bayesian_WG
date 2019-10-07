model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the blended model to your observed data. 
    y[k,j] ~ dnorm(mu[k,j],tau_obs)
    Temp[k,j] ~ dnorm(mu_T[k,j],tau_T_obs)

  }
  
  #### Process Model
  
  for(j in 2:max(season_weeks)){
    
    #process model for Gloeo
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1 + beta2*mu[k,j-1] + beta3*Temp[k,j] + beta4*Temp[k,j]^2 + yr[year_no[k]]
    
    #process model for temperature
    mu_T[k,j]~dnorm(T_mean,tau_proc_T)
  }
    
    #Loops through items in seasonal for-loop and defines initial conditions
    yr[k] ~ dnorm(0,tau_yr)
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
    mu_T[k,1]~dnorm(T_mean,tau_proc_T)
  
  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1) 
  beta2 ~ dnorm(beta.m2,beta.v2) 
  beta3 ~ dnorm(beta.m3,beta.v3) 
  beta4 ~ dnorm(beta.m4,beta.v4) 
  tau_yr ~ dgamma(0.01,0.01)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_T_obs ~ dgamma(0.01, 0.01) 
  
}