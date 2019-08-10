#Does the lambda need to be logged? 

model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    m[i]<- exp(mu[i])
  }
  #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(lambda[i],tau_add) 
    lambda[i]<- beta[1]*mu[i-1] + yr[year_no[i]]
    
  }
  
  #### Priors
  lambda[1]~ dnorm(x_ic, tau_ic)
  tau_add ~ dgamma(a_add,r_add)
  beta[1] ~ dnorm(beta.m,beta.v)
  tau_yr ~ dgamma(0.01,0.01)
  mu[1] ~ dgamma(1,1)
  
  #Loops through number of years and defines prior for each year 
  for(k in 1:max(year_no)) {
    yr[k] ~ dnorm(0,tau_yr)
  }
}