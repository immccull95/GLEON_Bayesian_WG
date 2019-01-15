##Response variable = Poisson
##Added Covariate to Random Walk State Space Model
##Model Cannot Capture Extreme Values

##clear environment, load packages.----
rm(list =ls())
library(rjags)
library(readr)

##Read in data and subset to include only Midge
All_Sites_Gloeo <- read_csv("Datasets/Sunapee/R Work/Level 1/All_Sites_Gloeo.csv")
View(All_Sites_Gloeo)
Data=subset(All_Sites_Gloeo, site=="Midge")

##Identify response variable and predictor(s)
y = round(Data$totalperL*141.372)  # conversion to count data
x=  Data$month #Covariate
as.numeric(x) 
n=length(y)
x.new=seq(2,10, 0.05)


RandomWalk = "
model{

#### Priors
b[1] ~ dnorm(x_ic,tau_ic) #b on log scale
b.new[1] ~ dnorm(x_ic, tau_ic)
tau_add ~ dgamma(a_add,r_add)

#### Data Model
for(i in 1:n){
mu[i] <- exp(b[i]) #mu on linear scale
y[i] ~ dpois(mu[i]) # variance = mean
}

#### Process Model
for(i in 2:n){
b[i] <- b[i-1]+beta[1]+beta[2]*x[i-1] 
}

for(i in 1:2){
beta[i] ~dnorm(0,0.001)
}

### Predictions

#### Data Model
for(j in 1:length(x.new)){
mu.new[j] <- exp(b.new[j]) #mu on linear scale
y.new[j] ~ dpois(mu.new[j]) # variance = mean
}

#### Process Model
for(j in 2:length(x.new)){
b.new[j] <- b.new[j-1]+beta[1]+beta[2]*x.new[j-1] 
}

for(i in 1:2){
beta.new[i] ~dnorm(0,0.001)
}
}
"

data <- list(y=y,n=n,x=x, x.new=x.new, x_ic=0.1,tau_ic=100,a_add=.001,r_add=.001)


# Initials
nchain = 3
#init <- list()


#modified for poisson model
#for(i in 1:nchain){
#  y.samp = sample(data$y,length(data$y),replace=TRUE)
#  init[[i]] <- list(tau_add=1/var(diff(y.samp)),b=log(y.samp+1))
#}



j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         #inits = init,
                         n.chains = nchain)

var.out   <- coda.samples (model = j.model,
                           variable.names = c("beta", "y.new"),
                           n.iter = 10000)


#Removing burn-in period for model run
burnin=1000
params <- window(var.out, start=burnin) 

#Convergence of parameters-- can skip over running these if you want
par(mar=c(1,1,1,1)) #changing margins to fit plots in window
plot(params) #I am just looking at the beta parameters here
gelman.plot(params) #suggests we might need a longer burn-in period

#Summary statistics
summary(params)
cor(as.matrix(params)) #The intercepts and slopes for each distribution are highly correlated, which I think we can account
#for in the model using a covariance matrix

#Plotting Credible Intervals
zj = jags.samples(j.model, variable.names = c("beta", "y.new"), n.iter= 10000, n.thin = 1)
b <- summary(zj$y.new, quantile, c(.025, .5, .975))$stat

par(mar=c(5.1, 4.1, 4.1, 2.1)) #changing margins back to default values
plot(x.new, b[2,],type = "l", xlab="Month", ylab="Predicted Gloeo Colony Counts")
lines(x.new, b[1,], lty = "dashed")
lines(x.new, b[3,], lty = "dashed")

