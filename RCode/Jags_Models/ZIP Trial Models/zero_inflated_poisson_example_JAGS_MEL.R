#building zero inflated data, fitting it with a zero inflated model.
#clear environment, load packages.----
rm(list=ls())
library(runjags)
library(coda)
library(rjags)
library(geosphere)

#simulate data.----
N <- 365*4 #4 years
N.new <- 365*4 #4 years

Nvec <- seq(1, N, 1)

#simulated air temperature with seasonality
#x1 <- runif(N, -10, 30) #no seasonality
xtemp1 <- seq(0,8*pi,length.out=N)
x1 <- sin(xtemp1+5)*20+10 #+5 iswhere curve starts on y-axis, *20 is modifier so the range is 40 rather than 2, +10 is offset from 0 as mean
x1.new <- sin(xtemp1+5)*20+10 #+5 iswhere curve starts on y-axis, *20 is modifier so the range is 40 rather than 2, +10 is offset from 0 as mean

#simulated water temperature with seasonality
#x2 <- runif(N, 4, 25) #no seasonality
xtemp2 <- seq(0,8*pi,length.out=N)
x2 <- sin(xtemp2+5)*12.5+15 #+5= where curve starts on y-axis, *12.5 is modifier so the range is 25 rather than 2, +15 is offset from 0 as mean
x2.new <- sin(xtemp2+5)*12.5+15 #+5= where curve starts on y-axis, *12.5 is modifier so the range is 25 rather than 2, +15 is offset from 0 as mean

#Daylength
daylength_hours=daylength(43.3802, 1:365) #input latitude and days of year
x3=c(daylength_hours, daylength_hours, daylength_hours, daylength_hours) #4 years of data
x3.new=c(daylength_hours, daylength_hours, daylength_hours, daylength_hours) #4 years of data

plot(Nvec, x1, col = "red", type = "l", main = "Simulated Air (Red) & Water (Blue) Temp & Daylength (Purple).")
lines(Nvec, x2, col = "blue", type = "l")
lines(Nvec, x3, col="purple", type="l")

#Lets specify a poisson distribution where the abundance depends on water temperature and an intercept.
#we're going to exponentiate because we use a log link in the model.
beta.pois.real <- c(-1,0.2)
y.pois <- exp(beta.pois.real[1] + x2*beta.pois.real[2])
hist(y.pois)

#Time to zero inflate. Zero inflation will also depend on day length and an intercept.
#I simulate the probabilty as a continuous value.
#I then send these values data through a logit transform to put it on the scale (0,1)
#setting the intercept to be negative so this is often zero.
beta.bern.real <- c(-2, 0.1)
y.prob <- beta.bern.real[1] + x3*beta.bern.real[2] #Air temp & day length are highly correlated so only including one
y.prob <- boot::inv.logit(y.prob)

#send through the binomial data model to get zero-one outcomes.
y.bern <- rbinom(N, 1, y.prob)
hist(y.bern)

#multiply the bernoulli observation (did we see algae or not?) by the poisson observation (if we saw algae, how much algae?)
#This is what y'all actually observe in the lake.
#take that outcome, send it through the poisson data model.
y.obs <- y.bern*y.pois
y.obs <- rpois(N, lambda = y.bern*y.pois)
plot(Nvec,y.obs, type = "p", main = "Simulated Gloeo data")
hist(y.obs, main = "Histogram of simulated Gloeo data")

#Multivariate normal matrices for beta priors
beta.bern.m <- as.vector(c(0,0))
beta.bern.v <- solve(diag(1E-03,2))
beta.pois.m <- as.vector(c(0,0))
beta.pois.v <- solve(diag(1E-03,2))

#set values in "winter" to NA (Nov.-Feb.)
#y.obs[c(1:60, 305:425, 670:790, 1035:1155, 1400:1460)] <- NA
#plot(Nvec,y.obs, type = "p", main = "Simulated Gloeo data with winter")


#set values in between "weekly sampling" to NA
#ind <- seq(1, length(y.obs), by=7)

#for (i in 1:length(y.obs)){
#  if(!(i %in% ind)){
#    y.obs[i] <- NA
#  }
#}
#plot(Nvec,y.obs, type = "p", main = "Simulated Gloeo data with winter & weekly obs.")


#Cool. We now have zero inflate data where we know all of the true parameters!

#Lets build a JAGS model that can recover those true parameters.----

jags.model <- "
model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
  }
  
  for(i in 2:N){
    
    #This blends the poisson and zero inflation models
    m[i] <- mu[i]*b[i] + 1E-10 #adding the tiny value is important (i forget why...)
    
    #this is the bernoulli outcome of the zero inflation
    b[i] ~ dbern(theta[i])
    
    #mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
    log(mu[i]) <- beta.pois[1]*m[i-1] + beta.pois[2]*x2[i]

    #theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
    logit(theta[i]) <- beta.bern[1]+ beta.bern[2]*x3[i]
  }
  
  #setup your priors. These are flat, uninformative priors.
  beta.bern ~ dmnorm(beta.bern.m,beta.bern.v)
  beta.pois ~ dmnorm(beta.pois.m,beta.pois.v)
  
  #Initial value
  m[1]=1

#Calculating predictions so we can plot prediction & credible intervals
for(j in 1:N.new){
  y.new[j] ~ dpois(m.new[j])
  }
for(j in 2:N.new){
  m.new[j] <- mu.new[j]*b.new[j] + 1E-10 
  b.new[j] ~ dbern(theta.new[j])
    log(mu.new[j]) <- beta.pois[1]*m.new[j-1] + beta.pois[2]*x2.new[j]
    logit(theta.new[j]) <- beta.bern[1]+ beta.bern[2]*x3.new[j]
}

m.new[1]=1

} #close model loop.
"

#setup JAGS data object.----
#N.pred = 2. one predictor is the intercept, the second is x ("temperature").
jags.data <- list(y = y.obs, x3=x3, x3.new=x3.new, x2=x2, x2.new=x2.new, N.new=N.new, N = N, 
                  beta.bern.m=beta.bern.m, beta.bern.v=beta.bern.v,
                  beta.pois.m=beta.pois.m, beta.pois.v=beta.pois.v,
                  b=ifelse(y.obs==0,0,1), b.new=ifelse(y.obs==0,0,1)) #I am not exactly sure what this ifelse statement is doing
                                          #But I found it in a ZIP example online
                                          #And when I remove it, the model takes hours to run 

#set up initial conditions 
#nchain=3
#init <- list()
#for(i in 1:nchain){
#  init[[i]] <- list(beta.pois= c(1,0.1,0.1), beta.bern=c(-3, 0.1))
#}

#fit the jags object using runjags.----
jags.out <- run.jags(    model = jags.model,
                          data = jags.data,
                         adapt =  10000,
                        burnin =  500,
                        sample = 20000,
                      n.chains = 3,
                     # inits=init,
                       monitor = c('beta.bern', 'beta.pois', 'y.new'))

#summarize output.
plot(jags.out, vars=c("beta.bern", "beta.pois"))
jags.out.mcmc <- as.mcmc.list(jags.out)
summary(jags.out, vars=c("beta.bern", "beta.pois"))
gelman.plot(jags.out.mcmc, vars=c("beta.bern", "beta.pois"))

#Credible intervals

out <- as.matrix(jags.out.mcmc)
ci <- apply(exp(out[,5:ncol(out)]),2,quantile,c(0.025,0.5,0.975))
par(mfrow=c(1,1))
par(mar=c(5,5,5,5)) 
plot(Nvec, log(ci[2,]),type = "l", xlab="Time", ylab="Predicted Gloeo Colony Counts (Log scale)")
hist(log(ci[2,]))
#lines(Nvec, log(ci[1,]), lty = "dashed")
#lines(Nvec, log(ci[3,]), lty = "dashed")

plot(log(ci[2,]), y.obs, xlab="Log predicted values", ylab="Raw observed values")
