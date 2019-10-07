#Forecast plug n play
#Author:Mary Lofton
#Date: 05OCT19

get_params <- function(model_name, forecast_type){
  
  prow = prow
  out = out
  
  ##DETERMINISTIC AND INITIAL CONDITIONS 
  if(forecast_type == "det" | forecast_type == "IC"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
      params <- list(sd_obs = 0, sd_proc = 0)
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 0, sd_proc = 0, sd_yr = 0)
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE))
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0)
    }
  }
  
  ##PROCESS UNCERTAINTY 
  if(forecast_type == "IC.P"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]))
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), sd_yr = 0)
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE))
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0)
    }
  }
  
  ##OBSERVATION UNCERTAINTY 
  if(forecast_type == "IC.P.O"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]))
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), sd_yr = 0)
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE))
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0)
    }
  }
  
  ##RANDOM EFFECTS UNCERTAINTY 
  if(forecast_type == "IC.P.O.R"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name == "Seasonal_AR" | model_name == "Seasonal_AR_Temperature"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }
    
    if(model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), sd_yr = 1/sqrt(out[prow,"tau_yr"]))
    }
  }
  
  ##PARAMETER UNCERTAINTY 
  if(forecast_type == "IC.P.O.Pa"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name == "Seasonal_RandomWalk_RandomYear"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }
    
    if(model_name == "Seasonal_AR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"])
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_S = 0)
    }
  }
  
  ##DRIVER UNCERTAINTY 
  if(forecast_type == "IC.P.O.Pa.D"){
    
    if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error" | model_name == "Seasonal_RandomWalk_RandomYear" | model_name == "Seasonal_AR"){
      params <- NULL
      print("This type of uncertainty is invalid for model_name.")
    }
    
    if(model_name == "Seasonal_AR_Temperature"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Temp_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
  }
  
  
  return(params)
  
}

forecast_gloeo <- function(model_name, params, settings){
  
  IC = settings$IC
  N_out = settings$N_out
  Nmc = settings$Nmc
  proc.model <- matrix(NA, Nmc, N_out) # setting up output 
  out <- matrix(NA, Nmc, N_out)
  ts = rbind(1:20,21:40)
  forecast_years = forecast_years
  week_avg = week_avg
  
  if(model_name == "Seasonal_RandomWalk"){
    
  for(k in 1:length(forecast_years)){
    gloeo_prev <- IC[,k]
    t <- ts[k,]
    
    #populate first week of season with IC
    if(k == 1){proc.model[,1] <- IC[,k]
    out[,1] <- IC[,k]} else {
      proc.model[,21] <- IC[,k]
      out[,21] <- IC[,k]
    }
    
    for(j in 2:max(season_weeks)){
      #process model
      proc.model[,t[j]] = rnorm(Nmc,gloeo_prev,params$sd_proc)
      #data model
      out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
      #update IC
      gloeo_prev <- out[,t[j]] # update IC 
    }}}
  
  if(model_name == "Seasonal_RandomWalk_Obs_error"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
      }
      
      for(j in 2:max(season_weeks)){
        #process model
        proc.model[,t[j]] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}

  if(model_name == "Seasonal_RandomWalk_RandomYear"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
      }
      
      #set random year effect
      yr = rnorm(Nmc,0,params$sd_yr)
      
      for(j in 2:max(season_weeks)){
        #process model
        gloeo_temp = gloeo_prev + yr
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
      }
      
      for(j in 2:max(season_weeks)){
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR_Temperature"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
      }
      
      for(j in 2:max(season_weeks)){
        #temp model
        Temp = rnorm(Nmc,week_avg[j],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR_Temp_and_Diff"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      Temp_prev = rnorm(Nmc,week_avg[1],params$sd_T)
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
      }
      
      for(j in 2:max(season_weeks)){
        #temp model
        Temp = rnorm(Nmc,week_avg[j],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp + params$beta4*(Temp-Temp_prev)
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC
        Temp_prev <- Temp
      }}}
  
  if(model_name == "Seasonal_AR_Schmidt"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
      }
      
      for(j in 2:max(season_weeks)){
        #temp model
        Schmidt = rnorm(Nmc,week_avg[j],params$sd_S)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Schmidt
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR_Schmidt_and_Diff"){
    
    for(k in 1:length(forecast_years)){
      gloeo_prev <- IC[,k]
      t <- ts[k,]
      Schmidt_prev = rnorm(Nmc,week_avg[1],params$sd_S)
      
      #populate first week of season with IC
      if(k == 1){proc.model[,1] <- IC[,k]
      out[,1] <- IC[,k]} else {
        proc.model[,21] <- IC[,k]
        out[,21] <- IC[,k]
        }
      
      for(j in 2:max(season_weeks)){
        #temp model
        Schmidt = rnorm(Nmc,week_avg[j],params$sd_S)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Schmidt + params$beta4*(Schmidt-Schmidt_prev)
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC
        Schmidt_prev <- Schmidt
      }}}
  
  
  return(out)
}

forecast_plot <- function(cal_years, forecast_years, is.forecast.ci, forecast.ci){
  
  rows <- (length(cal_years) + length(forecast_years))/2
  ps = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  qs = rbind(1:20,21:40)
  ci = ci
  pi = pi
  obs_pi = obs_pi
  N.cols <- c("black","red","green","blue","orange")
  trans <- 0.8       ## set transparancy
  
  par(mfrow = c(rows,2), oma = c(1,1,5,1), mar = c(4,6,1,0)+0.1,
      mgp = c(3,0.75,0))
  
  for (i in 1:length(cal_years)){
    
  plot(times[ps[i,]],ci[2,ps[i,]],type='n', ylab="Gloeo (colonies/L)", ylim = c(min(obs_pi[1,ps[i,]], na.rm = TRUE),max(obs_pi[3,ps[i,]], na.rm = TRUE)),
       main="",xlab = "", cex.lab = 3, cex.axis = 2.5)
  ciEnvelope(times[ps[i,]],obs_pi[1,ps[i,]],obs_pi[3,ps[i,]],col="gray")
  ciEnvelope(times[ps[i,]],pi[1,ps[i,]],pi[3,ps[i,]],col="yellow")
  ciEnvelope(times[ps[i,]],ci[1,ps[i,]],ci[3,ps[i,]],col="lightBlue")
  points(times[ps[i,]],ys[ps[i,]],pch="+",cex=2)
  legend("topleft",legend = cal_years[i], bty = "n", cex = 3)

  }
  
  if(!is.na(forecast_years[1])){
  
  for (l in 1:length(forecast_years)){
    
    if(is.forecast.ci == "y"){
      if(max(forecast.ci[3,qs[l,]], na.rm = TRUE) > max(forecast_ys, na.rm = TRUE)){
      forecast.ci <- forecast.ci
      lims <- c(min(forecast.ci[1,qs[l,]], na.rm = TRUE),max(forecast.ci[3,qs[l,]], na.rm = TRUE))}
      else{lims <- range(forecast_ys, na.rm = TRUE)}} 
    else {lims <- range(forecast_ys, na.rm = TRUE)}
    
    plot(forecast_times[qs[l,]],forecast_ys[qs[l,]],type='n', ylab="Gloeo (colonies/L)", ylim = lims,
         main="",xlab = "", cex.lab = 3, cex.axis = 2.5)
    if(is.forecast.ci == "y"){ciEnvelope(forecast_times[qs[l,]],forecast.ci[1,qs[l,]],forecast.ci[3,qs[l,]],col = "plum1")}
    lines(forecast_times[qs[l,]], exp(det.prediction[qs[l,]]), col="purple", lwd=5)
    points(forecast_times[qs[l,]],forecast_ys[qs[l,]],pch="+",cex=2)
    legend("topleft",legend = forecast_years[l], bty = "n", cex = 3)

  }}
  
  #title(main="Obs (+), Latent CI (blue), PI (green), Obs PI (grey), Mean Pred. (<>)",outer=T, cex = 3) 
  
}

make_varMat <- function(model_name){
  
  if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
    var.IC     <- apply(forecast.IC,2,var)
    var.IC.P    <- apply(forecast.IC.P,2,var)
    var.IC.P.O   <- apply(forecast.IC.P.O,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.O)
  }
  
  if(model_name == "Seasonal_RandomWalk_RandomYear"){
    var.IC     <- apply(forecast.IC,2,var)
    var.IC.P    <- apply(forecast.IC.P,2,var)
    var.IC.P.O   <- apply(forecast.IC.P.O,2,var)
    var.IC.P.O.R   <- apply(forecast.IC.P.O.R,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.O,var.IC.P.O.R)
  }
  
  if(model_name == "Seasonal_AR"){
    var.IC     <- apply(forecast.IC,2,var)
    var.IC.P    <- apply(forecast.IC.P,2,var)
    var.IC.P.O   <- apply(forecast.IC.P.O,2,var)
    var.IC.P.O.Pa   <- apply(forecast.IC.P.O.Pa,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.O,var.IC.P.O.Pa)
  }
  
  if(model_name == "Seasonal_AR_Temperature" | model_name == "Seasonal_AR_Temp_and_Diff" | model_name == "Seasonal_AR_Schmidt" | model_name == "Seasonal_AR_Schmidt_and_Diff"  ){
    var.IC     <- apply(forecast.IC,2,var)
    var.IC.P    <- apply(forecast.IC.P,2,var)
    var.IC.P.O   <- apply(forecast.IC.P.O,2,var)
    var.IC.P.O.Pa   <- apply(forecast.IC.P.O.Pa,2,var)
    var.IC.P.O.Pa.D   <- apply(forecast.IC.P.O.Pa.D,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.O,var.IC.P.O.Pa,var.IC.P.O.Pa.D)
  }
  
  return(vm)
}

plot_varMat <- function(model_name){
  
  N.cols <- c("black","red","green","blue","orange","yellow") ## set colors
  varMat = varMat
  V.pred.rel.2015 <- apply(varMat[,1:20],2,function(x) {x/max(x)})
  V.pred.rel.2016 <- apply(varMat[,21:40],2,function(x) {x/max(x)})
  V.pred.rel <- (V.pred.rel.2015 + V.pred.rel.2016) / 2
  
  if(nrow(varMat) == 3){
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], rep(1,20), col = N.cols[3])
    legend("bottomright", legend=c("Initial Cond","Process","Observation"), col=N.cols[1:3], lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  else if(nrow(varMat) == 5){
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[4])
    ciEnvelope(forecast_times[1:20], V.pred.rel[4,], V.pred.rel[5,], col = N.cols[5])
    ciEnvelope(forecast_times[1:20], V.pred.rel[5,], rep(1,20), col = N.cols[5])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Parameter","Driver"), col=N.cols[1:5], lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  else if(nrow(varMat) == 4 & model_name == "Seasonal_RandomWalk_RandomYear"){
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[6])
    ciEnvelope(forecast_times[1:20], V.pred.rel[4,], rep(1,20), col = N.cols[6])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Random Effects"), col=c(N.cols[1:3],N.cols[6]), lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  else {
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[4])
    ciEnvelope(forecast_times[1:20], V.pred.rel[4,], rep(1,20), col = N.cols[4])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Parameter"), col=N.cols[1:4], lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  return(V.pred.rel)
}

# plot_forecast_only <- function(model_name, forecast_years, forecast.ci){
#   
#   qs = rbind(1:20,21:40)
#   par(mfrow = c(3,1), oma = c(1,1,5,1), mar = c(3,3,1,0)+0.1,
#       mgp = c(2,0.5,0))
#   
#   for (l in 1:length(forecast_years)){
#     
#     
#       if(max(forecast.ci[3,qs[l,]], na.rm = TRUE) > max(forecast_ys, na.rm = TRUE)){
#         forecast.ci <- forecast.ci
#         lims <- c(min(forecast.ci[1,qs[l,]], na.rm = TRUE),max(forecast.ci[3,qs[l,]], na.rm = TRUE))}
#       else{lims <- range(forecast_ys, na.rm = TRUE)} 
#     
#     plot(forecast_times[qs[l,]],forecast_ys[qs[l,]],type='n', ylab="Gloeo (tot./L)", ylim = lims,
#          main="",xlab = "")
#     ciEnvelope(forecast_times[qs[l,]],forecast.ci[1,qs[l,]],forecast.ci[3,qs[l,]],col = "plum1")
#     lines(forecast_times[qs[l,]], exp(det.prediction[qs[l,]]), col="purple", lwd=2)
#     points(forecast_times[qs[l,]],forecast_ys[qs[l,]],pch="+",cex=0.8)
#     legend("topleft",legend = forecast_years[l], bty = "n")
#     
#   }
#   
#   plot_varMat(model_name = model_name)
#   
#   
# }
