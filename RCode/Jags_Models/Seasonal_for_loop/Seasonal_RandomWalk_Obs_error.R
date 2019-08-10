model{
  
  for(k in 1:max(year_no)){
    
    for(j in 1:max(season_weeks)){
      #this fits the blended model to your observed data. 
      y[k,j] ~ dnorm(mu[k,j],tau_obs)
    }
    #### Process Model
    for(j in 2:max(season_weeks)){
      mu[k,j]~dnorm(mu[k,j-1],tau_proc) 
    }
    #Loops through number of years and defines prior for each year
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
  }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  tau_obs ~ dgamma(a_obs, r_obs)
  
}