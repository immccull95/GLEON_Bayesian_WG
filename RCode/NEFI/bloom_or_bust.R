#Cleaned up Shannon Code for Eco-Forecasting Workshop July 2018
#BLOOM OR BUST: modeling Gloeotrichia in Lake Sunapee, NH

# change to your own!
setwd("C:/Users/immcc/Documents/GLEON_Bayesian_WG")#Ian
#setwd("~/Documents/GLEON_Bayesian_WG")#Jennie

#Load library
library(rjags)
library(coda)
library(tidyverse)

#Load data
gloeo = read.csv("Datasets/Sunapee/R Work/Level 1/All_Sites_Gloeo.csv")
wtr_temp = read.csv("Datasets/Sunapee/R Work/Level 1/watertemp_week.csv")

#Reformat water temp data
wtr_temp_long <- wtr_temp %>%
  gather(key=site, value = wtr_temp, coffin.mean:midge.obs) %>%
  separate(col=site,into = c("site","method")) %>%
  spread(key=method,value=wtr_temp) %>%
  arrange(year,site) %>% 
  rename(wtr_temp_max = max, wtr_temp_mean = mean, wtr_temp_median = median, wtr_temp_min = min)

#write.csv(wtr_temp_long, "Datasets/Sunapee/R Work/Level 1/wtr_temp_long_weekly_summary.csv")


##look at response variable - what data distributions are appropriate?


hist(gloeo$totalperL) #looks like zero inflated poisson
hist(gloeo$totalperL_diff)  # can be negative; looks more normal

# Random Walk Model - change data model to poisson

#simulate data.----
N <- 1000

#lets create a covariate, say temperature because I'm guessing that matters for algae.
x <- runif(N, 5, 25)

#we're going to exponentiate because we use a log link in the model.
beta.pois <- c(1,0.1)
y.pois <- exp(beta.pois[1] + x*beta.pois[2])

#Time to zero inflate. Zero inflation will also depend on temperature and an intercept.
#I am simulate the probabilty as a continuous value.
#I then send these values data through a logit transform to put it on the scale (0,1)
#setting the intercept to be negative so this is often zero.
beta.bern <- c(-3, 0.1)
y.prob <- beta.bern[1] + x*beta.bern[2]
y.prob <- boot::inv.logit(y.prob)

#send through the binomial data model to get zero-one outcomes.
y.bern <- rbinom(N, 1, y.prob)

#multiply the bernoulli observation (did we see algae or not?) by the poisson observation (if we saw algae, how much algae?)
#This is what y'all actually observe in the lake.
#take that outcome, send it through the poisson data model.
y.obs <- y.bern*y.pois
y.obs <- rpois(N, lambda = y.bern*y.pois)

data <- list(y=y.obs,n=length(y.obs),x_ic=0.1,tau_ic=100,a_add=.001,r_add=.001)

# Initials
nchain = 3
init <- list()

#OLD
# #for(i in 1:nchain){
#   y.samp = sample(y.obs,length(y.obs),replace=TRUE)
#   init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),tau_obs=1/var(log(y.samp)))
# }

#modified for poisson model
for(i in 1:nchain){
  y.samp = sample(data$y,length(data$y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp)),b=log(y.samp+1))
}

RandomWalk = "
model{

#### Priors
b[1] ~ dnorm(x_ic,tau_ic) #b on log scale
#mu[1] <- x_ic
tau_add ~ dgamma(a_add,r_add)

#### Data Model
for(i in 1:n){
mu[i] <- exp(b[i]) #mu on linear scale
y[i] ~ dpois(mu[i]) # variance = mean
}

#### Process Model
for(i in 2:n){
b[i] ~ dnorm(b[i-1],tau_add) 
}

}
"

#tau_obs ~ dgamma(a_obs,r_obs)


j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = nchain)

