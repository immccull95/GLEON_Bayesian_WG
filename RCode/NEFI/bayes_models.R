# collection of jags models 
require(rjags) 
require(runjags)

zip_model <- "
model{
for(i in 1:N){
#this fits the blended model to your observed data. 
y[i] ~ dpois(m[i])

#This blends the poisson and zero inflation models
m[i] <- mu[i]*b[i] + 1E-10 #adding the tiny value is important (i forget why...)

#this is the bernoulli outcome of the zero inflation
b[i] ~ dbern(theta[i])

#mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
log(mu[i]) <- beta.pois[1] + beta.pois[2]*x[i]

#theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
logit(theta[i]) <- beta.bern[1] + beta.bern[2]*x[i]
}

#setup your priors. These are flat, uninformative priors.
for(i in 1:N.pred){
beta.pois[i] ~ dnorm(0, 1E-3)
beta.bern[i] ~ dnorm(0, 1E-3)
}

} #close model loop.
"


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

RandomWalk_forecast = "
model{

#### Priors
b[1] ~ dnorm(x_ic,tau_ic) #b on log scale
#mu[1] <- x_ic
tau_add ~ dnorm(log(tau_med), tau_sd)

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



RandomWalk_temp = "
model{

#### Priors
b[1] ~ dnorm(x_ic,tau_ic) #b on log scale
#mu[1] <- x_ic
tau_add ~ dgamma(a_add,r_add)
beta.temp ~ dnorm(0,0.001)
beta0 ~ dnorm(0,0.001)

#### Data Model
for(i in 1:n){
mu[i] <- exp(b[i] + beta0 + beta.temp * temp[i]) #mu on linear scale
y[i] ~ dpois(mu[i]) # variance = mean
}

#### Process Model
for(i in 2:n){
b[i] ~ dnorm(b[i-1],tau_add) 
}

}
"



RandomWalk_logistic = "
model{

#### Priors
b[1] ~ dnorm(x_ic,tau_ic) #b on log scale
tau_add ~ dgamma(a_add,r_add)

#### Data Model
for(i in 1:n){
mu[i] <- exp(b[i])  #mu on linear scale
y[i] ~ dpois(mu[i]) # variance = mean
}

#### Process Model
for(i in 2:n){
meanb[i] <- b[i-1]
b[i] ~ dnorm(meanb[i],tau_add) 
}

}
"