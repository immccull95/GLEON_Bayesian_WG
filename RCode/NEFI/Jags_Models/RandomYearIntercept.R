#### Data Model
model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    
    #No blending but keeps mu on the log scale
    m[i] <- exp(mu[i]) 
    
  }
  
  #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(x[i],tau_add) #mu's here are on log scale
    x[i] <- mu[i-1] + beta[1] + yr[year_no[i]] # Adding year as a random effect + fixed effect of intercept
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  mu[1] ~ dgamma(1,1)
  beta[1] ~ dnorm(beta.m,beta.v)
  tau_yr ~ dgamma(0.01,0.01)
  
  #Loops through number of years and defines prior for each year 
  for(i in 1:max(year_no)){
    yr[i] ~ dnorm(0,tau_yr)
  }
}
