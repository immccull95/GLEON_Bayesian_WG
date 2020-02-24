model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the latent Gloeo to your observed Gloeo 
    #run this on logged data
    y[k,j] ~ dnorm(mu[k,j],tau_obs)
    
    #process model for max Schmidt
    Schmidt[k,j]~dnorm(week_min[j],tau_S_proc)
    
  }
  
  #### Process Model
  
  for(j in 2:max(season_weeks)){
    
    #process model for Gloeo
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1  + beta2*mu[k,j-1] + beta3*(Schmidt[k,j]-Schmidt[k,j-1]) 
    
    # #process model for temperature
    # Schmidt[k,j]~dnorm(week_max[j],tau_S_proc)

  }
    
    #Loops through items in seasonal for-loop and defines initial conditions
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
    #mu_S[k,1]~dnorm(x_S_ic,tau_S_ic) 
  
  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  beta1 ~ dnorm(beta.m1,beta.v1) 
  beta2 ~ dnorm(beta.m2,beta.v2) 
  beta3 ~ dnorm(beta.m3,beta.v3) 
  tau_obs ~ dgamma(a_obs,r_obs)
  #tau_S_obs ~ dgamma(0.01, 0.01) 
  tau_S_proc ~ dgamma(0.01, 0.01)
  
}