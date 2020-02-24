model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    
    m[i] <- exp(mu[i])
    
  }
  
  #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(lambda[i],tau_add) #mu's here are on log scale
    lambda[i] <- beta[1] + beta[2]*mu[i-1] + beta[3]*exp(Temp[i])
    
  }
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  beta ~ dmnorm(beta.m,beta.v) #this specifies a multivariate normal distribution for betas but you could also specify the parameters for each beta distribution separately
  
}