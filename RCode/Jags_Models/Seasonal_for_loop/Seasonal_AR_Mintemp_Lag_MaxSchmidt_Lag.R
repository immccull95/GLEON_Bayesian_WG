model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the latent Gloeo to your observed Gloeo 
    #run this on logged data
    y[k,j] ~ dnorm(mu[k,j],tau_obs)
    
    #observation model for Schmidt
    Schmidt[k,j]~dnorm(week_max[j],tau_S_proc)
    
  }
  
  #### Process Model
  
  for(j in 2:max(season_weeks)){
    
    #process model for Gloeo
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1  + beta2*mu[k,j-1] + beta3*Temp[k,j-1] + beta4*Schmidt[k,j-1]
    
    #process model for temperature
    Temp[k,j]~dnorm(week_min[j],tau_T_proc)

  }
    
    #Loops through items in seasonal for-loop and defines initial conditions
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
    #mu_T[k,1]~dnorm(x_T_ic,tau_T_ic) 
  
  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1) 
  beta2 ~ dnorm(beta.m2,beta.v2) 
  beta3 ~ dnorm(beta.m3,beta.v3)
  beta4 ~ dnorm(beta.m4,beta.v4) 
  tau_obs ~ dgamma(a_obs,r_obs)
  #tau_T_obs ~ dgamma(0.01, 0.01) 
  #tau_T_proc ~ dgamma(2.49e31, 2.49e31)
  #tau_S_proc ~ dgamma(3.55e31, 3.55e31)
  tau_T_proc ~ dgamma(0.01,0.01)
  tau_S_proc ~ dgamma(0.01,0.01)
  
  
}