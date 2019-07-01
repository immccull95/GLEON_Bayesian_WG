##Note for logistic model: beta[1]= 1+r and beta[2] = -r/k
##If we want more informative priors, we can calculate betas from component parts and put priors on r & k

model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(mu[i])
    #m[i]<- exp(mu[i])
  }
  #### Process Model
  for(i in 2:N){
   mu[i]~dnorm(lambda[i],tau_add) #allows mu to blow up to Inf if have too many NAs
   #lambda[i]<- beta[1]*mu[i-1] + beta[2]*mu[i-1]^2
   lambda[i] <- mu[i-1]*exp(r0*(1-(mu[i-1]/K)))
    
  }
  
  #### Priors
  mu[1]~ dpois(1)
  tau_add ~ dgamma(a_add,r_add)
  #beta ~ dmnorm(beta.m,beta.v)
  r0 ~ dunif(0.07,3.5)
  K ~ dnorm(18000,1/1000^2)
}
