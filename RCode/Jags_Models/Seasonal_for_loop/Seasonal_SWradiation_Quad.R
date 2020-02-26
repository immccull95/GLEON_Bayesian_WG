model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the latent Gloeo to your observed Gloeo 
    #run this on logged data
    y[k,j] ~ dnorm(mu[k,j],tau_obs)
    
    #observation model for temperature
    #Temp[k,j]~dnorm(mu_T[k,j],tau_T_obs)
    
  }
  
  #### Process Model
  
  for(j in 2:max(season_weeks)){
    
    #process model for Gloeo
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1  + beta2*mu[k,j-1] + beta3*SW[k,j] + beta4*(SW[k,j]^2)  
    
    #process model for temperature
    SW[k,j]~dnorm(week_avg[j],tau_SW_proc)

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
  #tau_D_proc ~ dgamma(6.70e-2, 1.38e-5)
  tau_SW_proc ~ dgamma(0.01,0.01)
  
}