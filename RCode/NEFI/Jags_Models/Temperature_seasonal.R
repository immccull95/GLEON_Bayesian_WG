model{
  
  #### Process Model
  
  for(k in 1:max(year_no)){
    
    for (j in 2:season_weeks){
    #this fits the blended model to your observed data. 
    y[j] ~ dpois(m[j]) #put data model in a separate but similar loop that uses y[1] (same k diff j)
    m[j] <- exp(mu[j])
      
    mu[j]~dnorm(lambda[j],tau_add) #mu's here are on log scale
    lambda[j] <- beta[1] + beta[2]*mu[j-1] + beta[3]*Temp[j] + yr[year_no[i]]
    }
    
    #Loops through number of years and defines prior for each year
    yr[k] ~ dnorm(0,tau_yr)
    mu[1] ~ dnorm(x_ic,tau_ic) #keep in mind you'll need to index like a matrix 
    lambda[1]~ dnorm(x_ic, tau_ic)
    
  }
  
  #### Priors
  tau_add ~ dgamma(a_add,r_add)
  beta ~ dmnorm(beta.m,beta.v)
  tau_yr ~ dgamma(0.01,0.01)

}