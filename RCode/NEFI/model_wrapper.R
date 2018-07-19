# wrapper for calling other functions 

source('RCode/NEFI/get_data.R') 
source('RCode/NEFI/bayes_models.R') 

# library(R2jags)

cal_time_start <- '1990-01-01' 
cal_time_end <- '2013-01-01' 
forecast_time_end <- '2016-01-01' 
sites <- c('midge') 

cal_data <- get_data(cal_time_start, cal_time_end, forecast_time_end, sites)$cal # get the data
forecast_data <- get_data(cal_time_start, cal_time_end, forecast_time_end, sites)$forecast 
all_data <- rbind(cal_data, forecast_data) %>%
  mutate(data_type = c(rep('cal_data', nrow(cal_data)), rep('forecast_data', nrow(forecast_data)))) %>%
  mutate(totalperL = case_when(data_type == 'cal_data' ~ totalperL,
                                  data_type == 'forecast_data' ~ NA_real_)) 

#setup JAGS data object.----
#N.pred = 2. one predictor is the intercept, the second is x ("temperature").
jags_data_cal <- list(y = cal_data$coloniesperL, x = cal_data$wtr_temp_mean, n = 1000)
jags_data_forecast <- list(y = all_data$coloniesperL, x = all_data$wtr_temp_mean, n = 1000)

z = round(cal_data$totalperL*141.3707)  ## questimate of liters]
data <- list(y=z,n=length(cal_data$totalperL),x_ic=0.1,tau_ic=100,a_add=.001,r_add=.001)


# Initials
nchain = 3
init <- list()


#modified for poisson model
for(i in 1:nchain){
  y.samp = sample(data$y,length(data$y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp)),b=log(y.samp+1))
}



j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = nchain)

# calibration 
cal <- jags.model (file = textConnection(model),
                       data = cal_data,
                       inits = init,
                       n.chains = 3)

forecast <- jags.model (file = textConnection(model),
                   data = all_data,
                   inits = init,
                   n.chains = 3)


# 





