model{
  
  for(k in 1:max(year_no)){
    
    for(j in 1:max(season_weeks)){
      #this fits the blended model to your observed data. 
      y[k,j] ~ dnorm(mu[k,j],tau_obs)
    }
    #### Process Model
    for(j in 2:max(season_weeks)){
      mu[k,j]~dnorm(x[k,j],tau_proc) 
      x[k,j] <- mu[k,j-1] + yr[year_no[k]]
    }
    #Loops through number of years and defines prior for each year
    yr[k] ~ dnorm(0,tau_yr)
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
      }
  #### Priors
  tau_proc ~ dgamma(a_proc,r_proc)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_yr ~ dgamma(0.01,0.01)
}