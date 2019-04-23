model{
  for(i in 1:N){
    y[i] ~ dpois(m[i])
    m[i] <- exp(x[i]) 
    x[i]~dnorm(mu[i],tau_add) #mu's and x's here are on log scale
    
  }
  #### Process Model
  for(i in 2:N){
    #temp2[i] =temp[i] * temp[i]
    #mu[i]~dnorm(mu[i-1],tau_add) #mu's here are on log scale
    muLO[i]=b0[1]*x[i-1]
    muHI[i]=beta[1]+ beta[2]*x[i-1] + beta[3]*exp(temp[i])
    mu[i]=ifelse(temp[i]>k,muHI[i],muLO[i])
  }
  
  #### Priors
  #k~dnorm(224,0.003) #prior from HE for DOY, based on 2008-16 lakes in ME, last max obs
  k~dnorm(10,0.1) #need to calculate this from HE data? 
  #k=10
  for(j in 1:3){
  beta[j]~dnorm(beta.m, beta.v)
  }
  
  b0[1]~dnorm(beta.m, beta.v)
  mu[1] ~ dnorm(x_ic,tau_ic) 
  tau_add ~ dgamma(a_add,r_add)
  
}