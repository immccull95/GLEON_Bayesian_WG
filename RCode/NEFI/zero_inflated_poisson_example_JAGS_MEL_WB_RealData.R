#building zero inflated data, fitting it with a zero inflated model.
#clear environment, load packages.----
rm(list=ls())
library(runjags)
library(coda)
library(rjags)
library(readr)
library(geosphere)
library(dplyr)

##Read in data and subset to include only Midge
All_Sites_Gloeo <- read_csv("Datasets/Sunapee/Level1/All_Sites_Gloeo.csv")
Datatemp=subset(All_Sites_Gloeo, site=="Midge")
plot(Datatemp$date, log(round(Datatemp$totalperL*141.3707)+.01), xlab="Date", ylab="log Gloeo Count")

##6/7 day NA model: Creating day of year vector
Year2005=seq(202, 268, 1)
Year2006=seq(180, 261, 1)
Year2007=seq(172, 263, 1)
Year2008=seq(157, 268, 1)
Year2009=seq(135, 278, 1)
Year2010=seq(105, 280, 1)
Year2011=seq(118, 279, 1)
Year2012=seq(47, 279, 1)
Year2013=seq(114, 282, 1)
Year2014=seq(149, 283, 1)
Year2015=seq(134, 285, 1)
Year2016=seq(147, 279, 1)
dayofyr2=c(Year2005, Year2006, Year2007, Year2008, Year2009, Year2010, Year2011, Year2012, Year2013, Year2014, Year2015, Year2016)

##6/7 day NA model: Creating date vector
Year2005=seq(as.Date("2005/7/21"), as.Date("2005/9/25"), "days")
Year2006=seq(as.Date("2006/6/29"), as.Date("2006/9/18"), "days")
Year2007=seq(as.Date("2007/6/21"), as.Date("2007/9/20"), "days")
Year2008=seq(as.Date("2008/6/05"), as.Date("2008/9/24"), "days")
Year2009=seq(as.Date("2009/5/15"), as.Date("2009/10/05"), "days")
Year2010=seq(as.Date("2010/4/15"), as.Date("2010/10/07"), "days")
Year2011=seq(as.Date("2011/4/28"), as.Date("2011/10/06"), "days")
Year2012=seq(as.Date("2012/2/16"), as.Date("2012/10/05"), "days")
Year2013=seq(as.Date("2013/4/24"), as.Date("2013/10/09"), "days")
Year2014=seq(as.Date("2014/5/29"), as.Date("2014/10/10"), "days")
Year2015=seq(as.Date("2015/5/14"), as.Date("2015/10/12"), "days")
Year2016=seq(as.Date("2016/5/26"), as.Date("2016/10/05"), "days")
date=c(Year2005, Year2006, Year2007, Year2008, Year2009, Year2010, Year2011, Year2012, Year2013, Year2014, Year2015, Year2016)
Full=data.frame(date)

##6/7 day NA model: Combining vectors  
Full$dayofyr2=dayofyr2

##6/7 day NA model: Adding NA's between sampling dates
Datatemp$date=format(as.Date(Datatemp$date), "%Y-%m-%d")
Full$date=format(as.Date(Full$date), "%Y-%m-%d")
Data <- merge(Datatemp, Full, by="date", all=T)

##Winter NA model
Year1=seq(202,365,1)
Year=seq(1,365,1)
YearLeap=seq(1,366,1)
AllYears=c(Year1, Year, Year, YearLeap, Year, Year, Year, YearLeap, Year, Year, Year, YearLeap)
AllDates=seq(as.Date("2005/07/21"), as.Date("2016/12/31"), "days")
Full2=data.frame(AllYears)
Full2$dayofyr3=Full2$AllYears
Full2$date=AllDates
Datatemp$date=format(as.Date(Datatemp$date), "%Y-%m-%d")
Full2$date=format(as.Date(Full2$date), "%Y-%m-%d")
Data3 <- merge(Datatemp, Full2, by="date", all=T)

##Identify predictor and response variable-- use this if you want data with no NAs
#y.obs = round(Datatemp$totalperL*141.3707)  #Estimated conversion to count data; will need to correct this later
#x=daylength(43.3802, Datatemp$dayofyr)

##Identify predictor and response variable-- use this if you want data with NAs for 
#6/7 days during sampling season
#y.obs = round(Data$totalperL*141.3707)  #Estimated conversion to count data; will need to correct this later
#x=daylength(43.3802, Data$dayofyr2) #input latitude and days of year

##Identify predictor and response variable-- use this if you want data with NAs for 
#6/7 days during sampling season and during winter
y.obs=round(Data3$totalperL*141.3707)
x=daylength(43.3802, Data3$dayofyr3)

##Counters
N=length(y.obs)
Nvec <- seq(1, N, 1)

##Multivariate normal matrices for beta priors
beta.bern.m <- as.vector(c(0,0))
beta.bern.v <- solve(diag(1E-03,2))
beta.pois.m <- as.vector(c(0,0))
beta.pois.v <- solve(diag(1E-03,2))

##Model
jags.model <- "
model{
  for(i in 1:N){
    #this fits the blended model to your observed data. 
    y[i] ~ dpois(m[i])
  }
  
  for(i in 2:N){
    
    #This blends the poisson and zero inflation models
    m[i] <- mu[i]*b[i] + 1E-10 #adding the tiny value is important 
    
    #this is the bernoulli outcome of the zero inflation
    b[i] ~ dbern(theta[i])
    
    #Adding process error to Poisson component
    mu[i]~ dnorm(log(lam[i]), tau.proc)

    #mu[i] is the linear combination of predictors and parameters for the poisson component of the model.
    log(lam[i]) <- beta.pois[1] + beta.pois[2]*m[i-1] 

    #theta[i] is the linear combination of predictors and paramters for the bernoulli component of the model.
    logit(theta[i]) <- beta.bern[1]+ beta.bern[2]*x[i]
  }
  
  #Flat priors
  beta.bern ~ dmnorm(beta.bern.m,beta.bern.v)
  beta.pois ~ dmnorm(beta.pois.m,beta.pois.v)
  tau.proc~dgamma(0.001,0.001)

  #Initial value
  m[1]~dpois(alpha)

} #close model loop.
"

##setup JAGS data object.----
jags.data <- list(y=y.obs, x=x,N = N, beta.bern.m=beta.bern.m, beta.bern.v=beta.bern.v,
                  beta.pois.m=beta.pois.m, beta.pois.v=beta.pois.v, alpha=100) 

##fit the jags object using runjags.----
jags.out <- run.jags(    model = jags.model,
                          data = jags.data,
                         adapt =  10000,
                        burnin =  500,
                        sample = 10000,
                      n.chains = 3,
                     #inits=init,
                       monitor = c('tau.proc', 'beta.bern', 'beta.pois', 'm'))

write.jagsfile(jags.out, file="Datasets/Sunapee/Jags Models/RealWeeklyWinterNA", 
              remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

##summarize output.
plot(jags.out, vars=c("beta.bern", "beta.pois", "tau.proc"))
jags.out.mcmc <- as.mcmc.list(jags.out)
summary(jags.out, vars=c("beta.bern", "beta.pois", "tau.proc"))
gelman.plot(jags.out.mcmc, vars=c("beta.bern", "beta.pois", "tau.proc"))

##Credible intervals for "m" 

out <- as.matrix(jags.out.mcmc)
ci <- apply((out[,6:ncol(out)]),2, quantile,c(0.025,0.5,0.975), na.rm=TRUE)
par(mfrow=c(1,1))
par(mar=c(5,5,5,5)) 
plot(Nvec, (ci[2,]),type = "l", xlab="Time", ylab="Gloeo Credible Intervals", log="y")
lines(Nvec, (ci[1,]), lty = "dashed")
lines(Nvec, (ci[3,]), lty = "dashed")
hist(ci[2,])

##m vs. y

plot(Nvec,log(ci[2, ]+.01),xlab="Time", log="y")
#lines(Nvec, ci[1,])
#lines(Nvec, ci[3,])
points(Nvec,log(y.obs+.01), col="red")

Mod=lm(y.obs~ci[2,]) ##Linear model comparing m and y, but doesn't account for NA's
summary(Mod)
plot(log(y.obs+.01), log(ci[2,]+.01), xlab="observed values", y="m 50% CI")

