# plug and play scripts 
# JAZ 2019-02-15


plug_n_play_data <- function(start_date, end_date, sites, model_timestep, fill_dates){
  
  sites = tolower(sites) 
  
  Data = get_data(cal_time_start = start_date, 
                  cal_time_end = end_date, 
                  model_timestep = model_timestep, # model timestep in days if filling in dates
                  fill_dates = fill_dates,  # T/F for filling in dates w/o observations with NA's 
                  sites = sites) %>%
    mutate(daylength = daylength(43.4802, date))
  
  # DL = Data$daylength
  # Temp= Data$watertemp_mean
  # time=as.character(Data$date)
  
  #Subset by Site
  midge=subset(Data, site=="midge" & year<2015)
  coffin=subset(Data, site=="coffin" & year<2015)
  fichter=subset(Data, site=="fichter" & year<2015)
  newbury=subset(Data, site=="newbury" & year<2015)
  
  #2) return correct site
  dat = eval(parse(text = sites))
  
  return(dat) 
}

jags_plug_ins <- function(model_name){

  #, x_ic = log(0.1), tau_ic = 100, a_add = 0.001, r_add = 0.001)
  #JAGS Plug-ins: Add each separate model here 
  #variable.names are variables you would like to plot for model diagnostics (e.g., excludes mu)
  #variable.names.out are all variables you would like to monitor in the jags run 
  
#Random Walk
  data.RandomWalk <- list(y=y, N=length(y),x_ic=log(0.1),tau_ic = 100, a_add = 0.001,r_add = 0.001)
  variable.names.RandomWalk<- c("tau_add")
  variable.namesout.RandomWalk<- c("tau_add", "mu")

#RandomWalkZip
  data.RandomWalkZip <- list(y=y, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001, alpha=1, epsilon=1)
  variable.names.RandomWalkZip<- c("tau_add", "theta")
  variable.namesout.RandomWalkZip<- c("tau_add", "mu", "theta")

#DayLength  
  data.DayLength <- list(y=y, beta.m=as.vector(c(0,0,0)), beta.v=solve(diag(1E-03,3)), DL=DL, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.DayLength <- c("tau_add", "beta")
  variable.namesout.DayLength <- c("tau_add", "beta", "mu")

#DayLengthQuad
  data.DayLengthQuad <- list(y=y, beta.m=as.vector(c(0,0,0,0)), beta.v=solve(diag(1E-03,4)), DL=DL, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.DayLengthQuad <- c("tau_add", "beta")
  variable.namesout.DayLengthQuad <- c("tau_add", "beta", "mu")

#TempExp
  data.TempExp <- list(y=y, beta.m=as.vector(c(0,0,0)), beta.v=solve(diag(1E-03,3)), Temp=Temp, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.TempExp <- c("tau_add", "beta")
  variable.namesout.TempExp <- c("tau_add", "beta", "mu")

#Temp_Quad
  data.Temp_Quad <- list(y=y, beta.m=as.vector(c(0,0,0,0)), beta.v=solve(diag(1E-03,4)), Temp=Temp, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.Temp_Quad <- c("tau_add", "beta")
  variable.namesout.Temp_Quad <- c("tau_add", "beta", "mu")

#Logistic
  data.Logistic <- list(y=y, beta.m=as.vector(c(0,0)), beta.v=solve(diag(1E-03,2)), N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.Logistic <- c("tau_add", "beta")
  variable.namesout.Logistic <- c("tau_add", "beta", "mu")

#Exponential
  data.Exponential <- list(y=y, beta.m=0, beta.v=1E-03, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.Exponential <- c("tau_add", "beta")
  variable.namesout.Exponential <- c("tau_add", "beta", "mu")

#RandomYear
  data.RandomYear <- list(y=y, year_no=year_no, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
  variable.names.RandomYear <- c("tau_add", "tau_yr")
  variable.namesout.RandomYear <- c("tau_add", "tau_yr","yr","mu", "x")

#RandomYearIntercept
  data.RandomYearIntercept <- list(y=y, year_no=year_no, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001, beta.m=0, beta.v=1E-03)
  variable.names.RandomYearIntercept <- c("tau_add", "tau_yr", "beta")
  variable.namesout.RandomYearIntercept <- c("tau_add", "tau_yr","yr","mu", "x", "beta")
  
  #data.ChangepointTempExp <- list(y=y, beta.m=beta.m, beta.v=beta.v, Temp=Temp, N=length(y),x_ic=log(0.1),tau_ic = 100,a_add = 0.001,r_add = 0.001)
 # variable.names.ChangepointTempExp <- c("tau_add", "beta", "k")
 # variable.namesout.ChangepointTempExp <- c("tau_add", "beta", "k", "mu")
  
  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  pred_obs= eval(parse(text = paste0('pred_obs.', model_name)))
  pred = eval(parse(text = paste0('pred.', model_name)))
  
  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout)) 
}







