# plug and play scripts 
# JAZ 2019-02-15


plug_n_play_data <- function(start_date, end_date, sites, model_timestep, fill_dates){
  
  sites = tolower(sites) 
  
  Data = get_data(cal_time_start = start_date, 
                  cal_time_end = end_date, 
                  model_timestep = 1, # model timestep in days if filling in dates
                  fill_dates = TRUE,  # T/F for filling in dates w/o observations with NA's 
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

jags_plug_ins <- function(model_name, y, beta.m, beta.v, Temp, DL, x_ic = log(0.1), tau_ic = 100, a_add = 0.001, r_add = 0.001){
  
  #JAGS Plug-ins: Add each separate model here 
  #variable.names are variables you would like to plot for model diagnostics (e.g., excludes mu)
  #variable.names.out are all variables you would like to monitor in the jags run 
  
  data.RandomWalk <- list(y=y, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.RandomWalk<- c("tau_add")
  variable.namesout.RandomWalk<- c("tau_add", "mu")
  
  data.RandomWalkZip <- list(y=y, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.RandomWalkZip<- c("tau_add")
  variable.namesout.RandomWalkZip<- c("tau_add", "mu")
  
  data.DayLength <- list(y=y, beta.m=beta.m, beta.v=beta.v, DL=DL, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.DayLength <- c("tau_add", "beta")
  variable.namesout.DayLength <- c("tau_add", "beta", "mu")
  
  data.DayLength_Quad <- list(y=y, beta.m=beta.m, beta.v=beta.v, DL=DL, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.DayLength <- c("tau_add", "beta")
  variable.namesout.DayLength <- c("tau_add", "beta", "mu")
  
  data.TempExp <- list(y=y, beta.m=beta.m, beta.v=beta.v, Temp=Temp, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.TempExp <- c("tau_add", "beta")
  variable.namesout.TempExp <- c("tau_add", "beta", "mu")
  
  data.Temp_Quad <- list(y=y, beta.m=beta.m, beta.v=beta.v, Temp=Temp, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.Temp_Quad <- c("tau_add", "beta")
  variable.namesout.Temp_Quad <- c("tau_add", "beta", "mu")
  
  data.Logistic <- list(y=y, beta.m=beta.m, beta.v=beta.v, N=length(y),x_ic=x_ic,tau_ic = tau_ic,a_add = a_add,r_add = r_add)
  variable.names.Logistic <- c("tau_add", "beta")
  variable.namesout.Logistic <- c("tau_add", "beta", "mu")
  
  
  data = eval(parse(text = paste0('data.', model_name)))
  variable.names = eval(parse(text = paste0('variable.names.', model_name)))
  variable.namesout = eval(parse(text = paste0('variable.namesout.', model_name)))
  
  return(list(data.model = data, variable.names.model = variable.names, variable.namesout.model = variable.namesout)) 
}







