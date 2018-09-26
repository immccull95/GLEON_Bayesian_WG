##Zero Inflated Mixture Model: Bernoulli determines 0, 1 (Gloeo Absence, Presence)
##If the data is treated as a 1, then the Poisson distribution is used to model the data
##This code also includes a linear covariate ("x"), which is currently designated as month of year 

##clear environment, load packages.----
rm(list =ls())
library(rjags)
library(readr)

##Read in data and subset to include only Midge
All_Sites_Gloeo <- read_csv("Datasets/Sunapee/R Work/Level 1/All_Sites_Gloeo.csv")
#View(All_Sites_Gloeo)
Data=subset(All_Sites_Gloeo, site=="Midge")

##Identify response variable and predictor(s)
y = round(Data$totalperL*141.372)  #Estimated conversion to count data; will need to correct this later
x=  Data$month #Covariate
as.numeric(x) 
N=length(y)
x.new=seq(2,10,0.05)

#JAGS model

jags.model <- "
model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])

    #This blends the poisson and zero inflation models
    m[i] <- mu[i]*b[i] + 1E-3 #adding the tiny value is important (i forget why...)
    
    #this is the bernoulli outcome of the zero inflation
    b[i] ~ dbern(one.minus.theta[i])
    
    #mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
    log(mu[i]) <- beta0.pois + beta1.pois*x[i]
    
    one.minus.theta[i] <- 1-theta[i]

    #theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
    logit(theta[i]) <- beta0.bern + beta1.bern*x[i]
  }
  
  #set up your priors. These are flat, uninformative priors.
    beta0.pois~ dnorm(0, 1E-03)
    beta1.pois~ dnorm(0, 1E-03)
    beta0.bern ~ dnorm(0, 1E-03)
    beta1.bern ~ dnorm(0, 1E-03)

  #Create model PREDICTIONS so we can calculate credible intervals later 
  #There may be an easier way to do this, but I'm using the same model as above, just calculating y.pred based on process model

  for(j in 1:length(x.new)){
   #this fits the blended model to your observed data. 
  y.pred[j] ~ dpois(m.new[j])

    #This blends the poisson and zero inflation models
  m.new[j] <- mu.new[j]*b.new[j] + 1E-3 #adding the tiny value is important (i forget why...)

    #this is the bernoulli outcome of the zero inflation
  b.new[j] ~ dbern(one.minus.theta.new[j])

    #mu[j] is the linear combination of predictors and parameters for the poisson component of the model.
  log(mu.new[j]) <- beta0.pois + beta1.pois*x.new[j]

  one.minus.theta.new[j] <- 1-theta.new[j]

    #theta[j] is the linear combination of predictors and paramters for the bernoulli component of the model.
  logit(theta.new[j]) <- beta0.bern + beta1.bern*x.new[j]
   }


} #close model loop.
"

#Set up JAGS data object

jags.data <- list(y = y, x = x, x.new=x.new, N = N, b=ifelse(y==0,0,1))
nchain=3

#JAGS Model 
j.model   <- jags.model(file = textConnection(jags.model),
                        data = jags.data,
                        #inits=init,
                        n.chains = nchain)

var.out.betas   <- coda.samples (model = j.model,
                             variable.names = c("beta0.pois", "beta1.pois", "beta0.bern", "beta1.bern"),
                             n.iter = 10000)
burnin=1000
params.betas <- window(var.out.betas, start=burnin)
par(mar=c(2,2,2,2))
plot(params.betas) #Trace plots well-mixed, posteriors unimodal
gelman.plot(params.betas) 
cor(as.matrix(params.betas)) #The intercepts and slopes for each distribution are highly correlated, which I think we can account
#for in the model using a covariance matrix

var.out.ypred <- coda.samples (model = j.model,
                               variable.names=c("beta0.pois", "beta1.pois", "beta0.bern", "beta1.bern","y.pred"), 
                               n.iter=30000)
burnin=10000
params.ypred <- window(var.out.ypred, start=burnin)
summary(params.ypred)

#Plotting Credible Intervals
zj = jags.samples(j.model, variable.names = c("y.pred"), n.iter= 30000, n.thin = 1)
b <- summary(zj$y.pred, quantile, c(.025, .5, .975))$stat

par(mfrow=c(1,1))
par(mar=c(5,5,5,5)) 
plot(x.new, b[2,],type = "l", xlab="Month", ylab="Predicted Gloeo Colony Counts")
lines(x.new, b[1,], lty = "dashed")
lines(x.new, b[3,], lty = "dashed")
