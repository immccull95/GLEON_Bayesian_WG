model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    Temp[i] ~ dnorm(tu[i],tau_add[3])
    
    m[i] <- exp(mu[i])
    
  }
  
  #### Process Model
  
  for(i in 2:N){
    mu[i]~dnorm(lambda[i],tau_add[1]) #mu's here are on log scale
    lambda[i] <- beta[1] + beta[2]*mu[i-1] + beta[3]*tu[i]
    tu[i]~dnorm(tu[i-1],tau_add[2])
  }
  
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dmgamma(a_add,r_add)
  beta ~ dmnorm(beta.m,beta.v)
  tu[1] ~ dnorm(x_ic_temp, tau_ic_temp)
  
}