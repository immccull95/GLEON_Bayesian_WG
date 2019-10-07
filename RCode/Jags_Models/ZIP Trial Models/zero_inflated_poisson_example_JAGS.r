#building zero inflated data, fitting it with a zero inflated model.
#clear environment, load packages.----
rm(list=ls())
library(runjags)


#simulate data.----
N <- 1000

#lets create a covariate, say temperature because I'm guessing that matters for algae.
x <- runif(N, 5, 25)

#Lets specify a poisson distribution where the abundance depends on temeperature and an intercept.
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

#Cool. We now have zero inflate data where we know all of the true parameters!

#Lets build a JAGS model that can recover those true parameters.----

jags.model <- "
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

#setup JAGS data object.----
#N.pred = 2. one predictor is the intercept, the second is x ("temperature").
jags.data <- list(y = y.obs, x = x, N = N, N.pred = 2)

#fit the jags object using runjags.----
jags.out <- run.jags(    model = jags.model,
                          data = jags.data,
                         adapt =  100,
                        burnin =  500,
                        sample = 1000,
                      n.chains = 3,
                       monitor = c('beta.pois','beta.bern'))

#summaryize output.
jags.sum <- summary(jags.out)

#compare fitted parameter values to true parameter values.----
#bernoulli parameters more likely to be the center of the distribution, as wel observe N binary outcomes.
#poisson parameters will be less likely to be at the center of the distribution as few observations are non-zero.
true <- c(beta.pois, beta.bern)
cbind(jags.sum[,1:3],true)

#plot, just for fun.
var.mat <- do.call(rbind, jags.out$mcmc)
par(mfrow = c(2,2))
for(i in 1:ncol(var.mat)){
  hist(var.mat[,i], main = colnames(var.mat)[i])
  abline(v = true[i], lwd = 2, col= 'purple')
}

