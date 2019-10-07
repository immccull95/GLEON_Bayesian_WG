model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    
    #This blends the poisson and zero inflation models
    m[i] <- exp(mu[i])*b[i] + 1E-10 #only when comparing them to the data, we need to exponentiate
    
    #this is the bernoulli outcome of the zero inflation
    b[i] ~ dbern(theta)
  }
  
  #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(mu[i-1],tau_add) #mu's here are on log scale
  }
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  theta~ dbeta(alpha, epsilon)
}