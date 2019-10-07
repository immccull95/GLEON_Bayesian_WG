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
    lambda[i]<- beta[1]*mu[i-1] 
    
  }
  
  #### Priors
  mu[1]~ dnorm(x_ic, tau_ic)
  tau_add ~ dgamma(a_add,r_add)
  beta[1] ~ dnorm(beta.m,beta.v)
}