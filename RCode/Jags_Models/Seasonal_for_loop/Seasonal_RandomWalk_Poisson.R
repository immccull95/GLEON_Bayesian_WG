model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the blended model to your observed data. 
    y[k,j] ~ dpois(m[k,j])
    
    #when comparing these values to the data, we need to exponentiate
    m[k,j] <- exp(mu[k,j])
  }
  #### Process Model
  for(j in 2:max(season_weeks)){
    mu[k,j]~dnorm(mu[k,j-1],tau_proc) #mu's here are on log scale
  }
    #Loops through number of years and defines prior for each year
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
  }
  #### Priors
  tau_proc ~ dgamma(a_add,r_add)
  
}