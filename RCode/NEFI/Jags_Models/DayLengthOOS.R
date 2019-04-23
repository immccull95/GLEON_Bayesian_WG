model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
    
    #This blends the poisson and zero inflation models
    m[i] <- exp(mu[i])
  }
  
  #### Process Model
  for(i in 2:N){
    mu[i]~dnorm(lambda[i],tau_add) #mu's here are on log scale
    lambda[i] <- beta[1] + beta[2]*mu[i-1] + beta[3]*DL[i]
    
  }
  
  ###OOS Model
  
  for(j in 1:N.oos){
    
    #OOS metrics
    diff.sq[j]<- (y.oos[j]- exp(y.hat[j]))^2
    density[j]<- dnorm(y.oos[j], exp(y.hat[j]), sqrt(1/tau_add)) #the dnorm input is sd, not tau
    
    #this fits the blended model to your observed data. 
    y.hat[j] ~ dpois(m.new[j])
    
    #This blends the poisson and zero inflation models
    m.new[j] <- exp(mu.new[j])
  }
  
  #### Process Model
  for(j in 2:N.oos){
    mu.new[j]~dnorm(lambda.new[j],tau_add) #mu's here are on log scale
    lambda.new[j] <- beta[1] + beta[2]*mu.new[j-1] + beta[3]*DL.oos[j]
    
  }
  
  MSPE<- sum(diff.sq)/N.oos
  PD<- prod(density)
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  mu.new[1] ~dnorm(x_ic, tau_ic)
  tau_add ~ dgamma(a_add,r_add)
  beta ~ dmnorm(beta.m,beta.v)
}