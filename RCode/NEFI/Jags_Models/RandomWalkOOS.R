
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
    
   ### OOS Model
    
  for(j in 1:N.oos){
    
    #OOS Metrics
    diff.sq[j]<- (y.oos[j]- exp(y.hat[j]))^2
    density[j]<- dnorm(y.oos[j], exp(y.hat[j]), sqrt(1/tau_add)) #the dnorm input is sd, not tau
    
    #this fits the blended model to your observed data. 
    y.hat[j] ~ dpois(m.new[j])
    
    #when comparing these values to the data, we need to exponentiate
    m.new[j] <- exp(mu.new[j]) 
    }
    
    #### Process Model
    for(j in 2:N.oos){
      mu.new[j]~dnorm(mu.new[j-1],tau_add) #mu's here are on log scale
    }
  
    MSPE<- sum(diff.sq)/N.oos
    PD<- prod(density)
  
  #### Priors
  mu[1] ~ dnorm(x_ic,tau_ic) 
  mu.new[1] ~dnorm(x_ic, tau_ic)
  tau_add ~ dgamma(a_add,r_add)
}
