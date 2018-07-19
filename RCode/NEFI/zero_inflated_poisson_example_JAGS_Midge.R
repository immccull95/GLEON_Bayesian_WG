#building zero inflated data, fitting it with a zero inflated model.
#clear environment, load packages.----
library(rjags)

Sites=All_Sites_Gloeo
midge=subset(All_Sites_Gloeo, site=="Midge")

y=midge$coloniesperL
x=midge$month
as.numeric(x)

#simulate data.----
N <- 230

#Lets build a JAGS model that can recover those true parameters.----

jags.model <- "
model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])

        #This blends the poisson and zero inflation models
    m[i] <- mu[i]*b[i] + 1E-3 #adding the tiny value is important (i forget why...)
    
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
#N.pred = 2. one predictor is the intercept, the second is x (month of year).

jags.data <- list(y = y, x = x, N = 230, N.pred = 2)

nchain=3


#init <- list()
#for(i in 1:nchain){
#init[[i]] <- list(beta.bern[1]= rnorm(1,0.5,2), beta.bern[2]= rnorm(1, 0.005, 1), beta.pois[1]= rnorm(1, -3, 2), 
#                  beta.pois[2]= rnorm(1, 0.5, 1))
#}

#init <- list()
#for(i in 1:nchain){
#init[[i]] <- list(beta.bern= c(rnorm(1,0.5,0.1), rnorm(1, 0.005,0.01)), beta.pois= c(rnorm(1, -3, 1), rnorm(1, 0.5, 0.25)))
#}

init <- list()
for(i in 1:nchain){
  init[[i]] <- list(beta.bern= c(.05, 0.0005), beta.pois= c(-.3, 0.05))
}


j.model   <- jags.model(file = textConnection(jags.model),
                        data = jags.data,
                        inits=init,
                        n.chains = nchain)


var.out   <- coda.samples (model = j.model,
                           variable.names = c("beta.pois", "beta.bern"),
                           n.iter = 3000)

#summaryize output.
jags.sum <- summary(var.out)

#compare fitted parameter values to true parameter values.----
#bernoulli parameters more likely to be the center of the distribution, as wel observe N binary outcomes.
#poisson parameters will be less likely to be at the center of the distribution as few observations are non-zero.
true <- c(beta.pois, beta.bern)
cbind(jags.sum[,1:3],true)
