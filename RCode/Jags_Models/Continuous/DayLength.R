model{
  for(i in 1:N){
    #this fits the model to your observed data. 
    y[i] ~ dpois(m[i])
    m[i] <- exp(mu[i])
  }
  
  #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(lambda[i],tau_add) #mu's here are on log scale
    lambda[i] <- beta[1] + beta[2]*mu[i-1] + beta[3]*DL[i] + yr[year_no[i]]
    
  }
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  beta ~ dmnorm(beta.m,beta.v)
  tau_yr ~ dgamma(0.01,0.01)
  lambda[1]~ dnorm(x_ic, tau_ic)
  
  #Loops through number of years and defines prior for each year 
  for(k in 1:max(year_no)) {
    yr[k] ~ dnorm(0,tau_yr)
  }
}