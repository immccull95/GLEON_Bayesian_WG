#Cleaned up Shannon Code for Eco-Forecasting Workshop July 2018
#BLOOM OR BUST
#IAN's FAULT

setwd("~/Documents/GLEON_Bayesian_WG/Datasets/Sunapee/R Work/Level 1")

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

write.csv(wtr_temp_long, "wtr_temp_long_weekly_summary.csv")


##look at response variable - what data distributions are appropriate?

hist(data$totalperL) 
hist(data$totalperL_diff)  # can be negative


# Model - change data model to poisson

RandomWalk = "
model{

#### Data Model
for(i in 1:n){
y[i] ~ dpois(x[i],tau_obs)
}

#### Process Model
for(i in 2:n){
x[i]~dnorm(x[i-1],tau_add) 
}

#### Priors
x[1] ~ dnorm(x_ic,tau_ic) 
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"