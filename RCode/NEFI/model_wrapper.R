# wrapper for calling other functions 

source('RCode/NEFI/get_data.R') 
source('RCode/NEFI/bayes_models.R') 

library(R2jags)

cal_time_start <- '1990-01-01' 
cal_time_end <- '2013-01-01' 
forecast_time_end <- '2016-01-01' 
sites <- c('coffin') 

data <- get_data(cal_time_start, cal_time_end, forecast_time_end, sites) # get the data

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



j.model <- jags.model (file = textConnection(model),
                       data = data,
                       inits = init,
                       n.chains = 3)
# call the model 






