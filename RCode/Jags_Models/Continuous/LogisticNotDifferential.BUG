#Logistic model in a not-differential form - hopefully runs faster
#Author: Leah Johnson
#Adapted for GLEON project by Mary Lofton, 12APR19

model {
  
  for (i in 1:N) {
      
      Y[i] ~ dpois(mu[i])
      mu[i] <- K*Y0/(Y0+(K-Y0)*exp(-r*t[i])) #t in our case is the season_week
      
  }

  r~dexp(2) #exponential distribution describes time between events in a Poisson point process
  K ~ dunif(7000, 30000)
  Y0 ~ dbern(0.1)
  tau<-1/sigma^2
  sigma ~ dexp(0.1)

}
