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




