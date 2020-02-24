model{
  for(k in 1:max(year)){
  #### Data model
  for(j in 1:max(week_of_sampling_season)){
    #this fits the latent G. echinulata density to your observed G. echinulata 
    y[k,j] ~ dnorm(mu[k,j],tau_obs) }
  #### Process Model
  for(j in 2:max(week_of_sampling_season)){
    #process model for G. echinulata
    mu[k,j]~dnorm(lambda[k,j],tau_proc) 
    lambda[k,j] <- beta1  + beta2*mu[k,j-1] + beta3*Environmental_driver[k,j] 
    #gap-filling model for driver data - using 5-year average for that week
    Environmental_driver[k,j]~dnorm(week_avg[j],tau_obs_driver)} #end j loop
  #### Define initial conditions; x = mean, tau = precision
    mu[k,1] ~ dnorm(x_ic,tau_ic)} #end k loop
  #### Priors
  #priors for error terms
  tau_proc ~ dgamma(a_proc,r_proc) #a = slope of gamma distribution; b = rate
  tau_obs ~ dgamma(a_obs,r_obs)
  #priors for parameters
  beta1 ~ dnorm(x_beta1,tau_beta1) #x = slope, tau = precision
  beta2 ~ dnorm(x_beta2,tau_beta2) 
  beta3 ~ dnorm(x_beta3,tau_beta3) 
  #informed condition for gap-filling driver data (not really a prior)
  tau_obs_driver ~ dgamma(a_driver, b_driver)} #end model