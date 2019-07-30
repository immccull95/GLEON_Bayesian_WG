model{
  
  for(k in 1:max(year_no)){
  
  for(j in 1:max(season_weeks)){
    #this fits the blended model to your observed data. 
    y[k,j] ~ dnorm(mu[k,j],1/sd_obs^2)

  }
  
  #### Process Model
  
  for(j in 2:max(season_weeks)){
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1 + beta2*mu[k,j-1] + beta3*Temp[k,j] + yr[year_no[k]]
  }
    
    #Loops through number of years and defines prior for each year
    yr[k] ~ dnorm(0,tau_yr)
    mu[k,1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
    lambda[k,1]~ dnorm(x_ic, tau_ic)
  
  }
  #### Priors
  tau_proc ~ dgamma(a_add,r_add)
  beta1 ~ dnorm(beta.m1,beta.v1) 
  beta2 ~ dnorm(beta.m2,beta.v2) 
  beta3 ~ dnorm(beta.m3,beta.v3) 
  tau_yr ~ dgamma(0.01,0.01)
  sd_obs ~ dnorm(1.79,1/6.23^2)
  
  #Loops through number of years and defines prior for each year 
  
  
}