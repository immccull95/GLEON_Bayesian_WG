# wrapper for calling other functions 

source('RCode/NEFI/get_data.R') 
source('RCode/NEFI/bayes_models.R') 

# library(R2jags)

cal_time_start <- '2010-05-01' 
cal_time_end <- '2010-10-01' 
forecast_time_end <- '2016-01-01' 
sites <- c('midge') 
model_timestep <- 3 # number of days between model estimates 

# have to figure out if we want regular time steps; if so, some model timesteps will get rid of data (e.g. 3 day timestep will not use a lot of weekly data)

data <- get_data(cal_time_start, cal_time_end, forecast_time_end, model_timestep, sites) # get the data
cal_data <- data$cal 
forecast_data <- data$forecast 

all_data <- rbind(cal_data, forecast_data) %>%
  mutate(data_type = c(rep('cal_data', nrow(cal_data)), rep('forecast_data', nrow(forecast_data)))) %>%
  mutate(totalperL = case_when(data_type == 'cal_data' ~ totalperL,
                                  data_type == 'forecast_data' ~ NA_real_)) %>%
  dplyr::filter(!duplicated(date))

# all_data$wtr_temp_mean <- ifelse(is.na(all_data$wtr_temp_mean), mean(all_data$wtr_temp_mean,na.rm = T), all_data$wtr_temp_mean)

z = round(all_data$totalperL*141.3707)  ## questimate of liters]
data <- list(y=z, n=length(all_data$totalperL),x_ic=0.1,tau_ic=100,a_add=.001,r_add=.001)

n.iter = 1000 # cutting down to save time and storage 
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

# j.model   <- jags.model (file = textConnection(RandomWalk_logistic),
#                          data = data,
#                          inits = init,
#                          n.chains = nchain)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c('mu' ,'tau_add'),
                            n.iter = n.iter)

time.rng = c(1,length(data$y)) ## adjust to zoom in and out
time = seq(time.rng[1], time.rng[2])
out <- as.matrix(jags.out)
tau.cols <- grep("^t",colnames(out)) ## grab all columns that start with the letter x
tau_med <- median(out[,tau.cols])
tau_sd <- sd(out[,tau.cols])

#### forecasting 
forecast_times <- forecast_data$date[!is.na(forecast_data$totalperL)]

for(m in 1:length(forecast_times)){
  cal_time_start <- '1990-01-01' 
  cal_time_end <- forecast_times[m] 
  forecast_time_end <- '2016-01-01'

  cal_data <- get_data(cal_time_start, cal_time_end, forecast_time_end, sites)$cal # get the data
  forecast_data <- get_data(cal_time_start, cal_time_end, forecast_time_end, sites)$forecast 
  all_data <- rbind(cal_data, forecast_data) %>%
    mutate(data_type = c(rep('cal_data', nrow(cal_data)), rep('forecast_data', nrow(forecast_data)))) %>%
    mutate(totalperL = case_when(data_type == 'cal_data' ~ totalperL,
                                 data_type == 'forecast_data' ~ NA_real_)) %>%
    dplyr::filter(!duplicated(date))
  
  
  z = round(all_data$totalperL*141.3707)  ## questimate of liters]
  
  a_add = tau_med^2/tau_sd^2 # transform to gamma distribution 
  r_add = tau_med / tau_sd^2
  
  data <- list(y=z,n=length(all_data$totalperL),x_ic=0.1,tau_ic=100,a_add=a_add,r_add=r_add)
  
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
  
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c('mu' ,'tau_add'),
                              n.iter = n.iter)
  
  
  time.rng = c(1,length(data$y)) ## adjust to zoom in and out
  time = seq(time.rng[1], time.rng[2])
  out <- as.matrix(jags.out)
  tau.cols <- grep("^t",colnames(out)) ## grab all columns that start with the letter t
  tau_med <- median(out[,tau.cols])
  tau_sd <- sd(out[,tau.cols])
  
  cur_out <- as.matrix(jags.out)
  x.cols <- grep("^m",colnames(cur_out)) ## grab all columns that start with the letter m
  ci <- apply(cur_out[,x.cols],2,quantile,c(0.025,0.5,0.975))
  
  # assign(paste0('jags.out',m), jags.out)
  assign(paste0('ci_',m), ci)
  
  print(paste(m, 'of', length(forecast_times)))
}

# 
# t_col <- function(color, percent = 50, name = NULL) {
#   #	  color = color name
#   #	percent = % transparency
#   #	   name = an optional name for the color
#   ## Get RGB values for named color
#   rgb.val <- col2rgb(color)
#   ## Make new color using input color as base and alpha set by transparency
#   t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
#                max = 255,
#                alpha = (100-percent)*255/100,
#                names = name)
#   ## Save the color
#   invisible(t.col)
#   
# }
# 
# windows()
# ### plotting 
# cur <- eval(parse(text = paste0('jags.out',1)))
# cur <- eval(parse(text = 'jags.out'))
# 
# cur <- as.matrix(cur)
# x.cols <- grep("^m",colnames(cur)) ## grab all columns that start with the letter x
# ci <- apply(cur[,x.cols],2,quantile,c(0.025,0.5,0.975))
# 
# med = ci[2,]
# med = med[!is.infinite(med)]
# ylim = range(data$y, na.rm = T)
# 
# # ylim = c(0,10)
# # xlim = c(0,10)
# 
# time.rng = c(1,ncol(ci)) ## adjust to zoom in and out
# time = seq(time.rng[1], time.rng[2])
# 
# plot(time,ci[2,],ylim=ylim,ylab="",xlim=time[time.rng], type='l', col = t_col('black', percent = 90))
# ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=t_col('blue', percent = 9))
# lines(time,ci[2,], col = t_col('black', percent = 90))
# 
# for(q in 2:length(forecast_times)){
#   cur <- eval(parse(text = paste0('jags.out',q)))
#   
#   cur <- as.matrix(cur)
#   x.cols <- grep("^m",colnames(cur)) ## grab all columns that start with the letter x
#   ci <- apply(cur[,x.cols],2,quantile,c(0.025,0.5,0.975))
#   
#   med = ci[2,]
#   med = med[!is.infinite(med)]
#   ylim = range(med)
#   
#   time.rng = c(1,ncol(ci)) ## adjust to zoom in and out
#   time = seq(time.rng[1], time.rng[2])
#   
#   ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=t_col('blue', percent = 99))
#   lines(time,ci[2,], col = t_col('black', percent = 80))
# }
# 
# lines(time,data$y,pch="+",cex=0.5, col= 'red')
# 
