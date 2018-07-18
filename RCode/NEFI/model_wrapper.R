# wrapper for calling other functions 

source('RCode/NEFI/get_data.R') 
source('RCode/NEFI/bayes_models.R') 

library(R2jags)

cal_time_start <- '1990-01-01' 
cal_time_end <- '2013-01-01' 
forecast_time_end <- '2016-01-01' 
sites <- c('1') 

data <- get_data(cal_time_start, cal_time_end, forecast_time_end, sites) # get the data

init <- 1 



j.model <- jags.model (file = textConnection(model),
                       data = data,
                       inits = init,
                       n.chains = 3)
# call the model 






