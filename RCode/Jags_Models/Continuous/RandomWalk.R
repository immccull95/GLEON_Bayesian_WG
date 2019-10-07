#### Data Model
  model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    
   
    #when comparing these values to the data, we need to exponentiate
    m[i] <- exp(mu[i]) 
  }

   #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(mu[i-1],tau_add) #mu's here are on log scale
  }
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
}
