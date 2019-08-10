##Zero Inflated Mixture Model: Bernoulli determines 0, 1 (Gloeo Absence, Presence)
##If the data is treated as a 1, then the Poisson distribution is used to model the data
##This code also includes a linear covariate ("x"), which is currently designated as month of year 

##clear environment, load packages.----
library(rjags)
library(readr)

##Read in data and subset to include only Midge
All_Sites_Gloeo <- read_csv("Datasets/Sunapee/R Work/Level 1/All_Sites_Gloeo.csv")
View(All_Sites_Gloeo)
Data=subset(All_Sites_Gloeo, site=="Midge")

##Identify response variable and predictor(s)
y = round(Data$totalperL*141.3707)  #Estimated conversion to count data; will need to correct this later
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
  }

  for(i in 2:N){

    #This blends the poisson and zero inflation models
    m[i] <- mu[i-1]*b[i-1] + 1E-3 #adding the tiny value is important (i forget why...)
    
    #this is the bernoulli outcome of the zero inflation
    b[i-1] ~ dbern(theta[i-1])
    
    #mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
    log(mu[i-1]) <- beta.pois[1] + beta.pois[2]*x[i-1]
    
    #theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
    logit(theta[i-1]) <- beta.bern[1] + beta.bern[2]*x[i-1]
  }
#set up your priors. These are flat, uninformative priors.
  for(i in 1:N.pred){
beta.pois[i] ~ dnorm(0, 1E-3)
beta.bern[i] ~ dnorm(0, 1E-3)
}

m[1]~dpois(10)

#Create model PREDICTIONS so we can calculate credible intervals later 
  #There may be an easier way to do this, but I'm using the same model as above, just calculating y.pred based on process model

  for (j in 1:length(x.new)){
   #this fits the blended model to your observed data. 
    y.pred[j] ~ dpois(m.new[j])
  }

  for (j in 2:length(x.new)){

    #This blends the poisson and zero inflation models
    m.new[j] <- mu.new[j-1]*b.new[j-1] + 1E-3 #adding the tiny value is important (i forget why...)

    #this is the bernoulli outcome of the zero inflation
    b.new[j-1] ~ dbern(theta.new[j-1])

    #mu[j] is the linear combination of predictors and parameters for the poisson component of the model.
    log(mu.new[j-1]) <- beta.pois[1] + beta.pois[2]*x.new[j-1]

    #theta[j] is the linear combination of predictors and paramters for the bernoulli component of the model.
    logit(theta.new[j-1]) <- beta.bern[1] + beta.bern[2]*x.new[j-1]
  }

m.new[1]~dpois(10)


}
"
  
#Set up JAGS data object
#N.pred = 2 because one predictor is the intercept, the second is x (month of year).

jags.data <- list(y = y, x.new=x.new, x = x, N = N, N.pred = 2)
nchain=3

#JAGS Model 
j.model   <- jags.model(file = textConnection(jags.model),
                        data = jags.data,
                        #inits=init,
                        n.chains = nchain)


var.out   <- coda.samples (model = j.model,
                           variable.names = c("beta.pois", "beta.bern", "y.pred"),
                           n.iter = 10000)


#Removing burn-in period for model run
burnin=1000
params <- window(var.out, start=burnin) 

#Convergence of parameters-- can skip over running these if you want
#par(mar=c(1,1,1,1)) #changing margins to fit plots in window
plot(params) #I am just looking at the beta parameters here
gelman.plot(params) 

#Summary statistics
summary(params)
cor(as.matrix(params)) #The intercepts and slopes for each distribution are highly correlated, which I think we can account
                       #for in the model using a covariance matrix

#Plotting Credible Intervals
zj = jags.samples(j.model, variable.names = c("beta.pois", "beta.bern", "y.pred"), n.iter= 10000, n.thin = 1)
b <- summary(zj$y.pred, quantile, c(.025, .5, .975))$stat

par(mar=c(5.1, 4.1, 4.1, 2.1)) #changing margins back to default values
plot(x.new, b[2,],type = "l", xlab="Month", ylab="Predicted Gloeo Colony Counts")
lines(x.new, b[1,], lty = "dashed")
lines(x.new, b[3,], lty = "dashed")

#The plot above looks strange (why the widening CI?) until you see the plot of the original data
plot(Data$month, y, xlab="Month", ylab="Observed Gloeo Colony Counts") 

