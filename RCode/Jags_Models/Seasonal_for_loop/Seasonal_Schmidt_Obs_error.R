model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the blended model to your observed data. 
    y[k,j] ~ dnorm(mu[k,j],tau_obs)
    Schmidt[k,j] ~ dnorm(mu_S[k,j],tau_S_obs)

  }
  
  #### Process Model
  
  for(j in 2:max(season_weeks)){
    
    #process model for Gloeo
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1 + beta2*mu[k,j-1] + beta3*Schmidt[k,j] + yr[year_no[k]]
    
    #process model for temperature
    mu_S[k,j]~dnorm(S_mean,tau_S_proc)
  }
    
    #Loops through items in seasonal for-loop and defines initial conditions
    yr[k] ~ dnorm(0,tau_yr)
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
    lambda[k,1]~ dnorm(x_ic, tau_ic)
    mu_S[k,1]~dnorm(S_mean,tau_S_proc)
  
  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1) 
  beta2 ~ dnorm(beta.m2,beta.v2) 
  beta3 ~ dnorm(beta.m3,beta.v3) 
  tau_yr ~ dgamma(0.01,0.01)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_S_obs ~ dgamma(0.01, 0.01)

}