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
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE), sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE), sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
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
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 0, sd_proc = 0, beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_W = 0)
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
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
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
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 0, sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_W = 0)
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
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE), beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     beta5 = mean(out[,grep("beta5",colnames(out))],na.rm = TRUE),sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
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
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_S = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     beta4 = mean(out[,grep("beta4",colnames(out))],na.rm = TRUE),sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = mean(out[,grep("beta1",colnames(out))],na.rm = TRUE),
                     beta2 = mean(out[,grep("beta2",colnames(out))],na.rm = TRUE), beta3 = mean(out[,grep("beta3",colnames(out))],na.rm = TRUE),
                     sd_W = 0)
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
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 0)
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_SW = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_D = 0)
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"], sd_D = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"], sd_SW = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_W = 0, sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 0)
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
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
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_S = 0, sd_T = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_T = 0, sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"], beta4 = out[prow,"beta4"],
                     sd_T = 0, sd_W = 0)
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_P = 0)
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_L = 0)
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],sd_W = 0)
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
    
    if(model_name == "Seasonal_AR_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_SWradiation_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_SW = 1/sqrt(out[prow,"tau_SW_proc"]))
    }
    
    if(model_name == "Seasonal_DayLength_Quad"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_D = 1/sqrt(out[prow,"tau_D_proc"]))
    }
    
    if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"],sd_D = 1/sqrt(out[prow,"tau_D_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     beta5 = out[prow,"beta5"],sd_SW = 1/sqrt(out[prow,"tau_SW_proc"]), sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_W = 1/sqrt(out[prow,"tau_W_proc"]), sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Schmidt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]))
    }
    
    if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
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
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_S = 1/sqrt(out[prow,"tau_S_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_L = 1/sqrt(out[prow,"tau_L_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],beta4 = out[prow,"beta4"],
                     sd_W = 1/sqrt(out[prow,"tau_W_proc"]), sd_T = 1/sqrt(out[prow,"tau_T_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Ppt"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_P = 1/sqrt(out[prow,"tau_P_proc"]))
    }
    
    if(model_name == "Seasonal_AR_PAR"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_L = 1/sqrt(out[prow,"tau_L_proc"]))
    }
    
    if(model_name == "Seasonal_AR_UnderwaterLight"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_L = 1/sqrt(out[prow,"tau_L_proc"]))
    }
    
    if(model_name == "Seasonal_AR_Wnd"){
      params <- list(sd_obs = 1/sqrt(out[prow,"tau_obs"]), sd_proc = 1/sqrt(out[prow,"tau_proc"]), beta1 = out[prow,"beta1"],
                     beta2 = out[prow,"beta2"], beta3 = out[prow,"beta3"],
                     sd_W = 1/sqrt(out[prow,"tau_W_proc"]))
    }
  }
  
  
  return(params)
  
}

forecast_gloeo <- function(model_name, params, settings){
  
  IC = settings$IC
  IC_S = settings$IC_S
  N_out = settings$N_out
  Nmc = settings$Nmc
  proc.model <- matrix(NA, Nmc, N_out) # setting up output 
  out <- matrix(NA, Nmc, N_out)
  ts = rbind(1:20,21:40)
  week_avg = week_avg
  week_min = week_min
  week_max = week_max
  week_num = week_num
  obs_data = obs_data
  colnums = colnums

  if(model_name == "Seasonal_RandomWalk"){
    if(week_num %in% c(1:16,21:36)){
    for(j in 1:5){
      #set initial conditions
      if(j == 1){gloeo_prev <- IC}
      #process model
      proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
      #data model
      out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
      #update IC
      gloeo_prev <- out[,j]
    }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
        #set initial conditions
        gloeo_prev <- IC
        #process model
        proc.model[,1] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
      }
  }
  
  if(model_name == "Seasonal_RandomWalk_Obs_error"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        proc.model[,j] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
        #set initial conditions
        gloeo_prev <- IC
        #process model
        proc.model[,1] = rnorm(Nmc,gloeo_prev,params$sd_proc)
        #data model
        out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
      }
  }

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
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
        #set initial conditions
        gloeo_prev <- IC
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev
        proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
      }
  }
  
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
  
  if(model_name == "Seasonal_AR_Mintemp"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
        #set initial conditions
        gloeo_prev <- IC
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
      }
  }
  
  
  if(model_name == "Seasonal_AR_MinSchmidt_Diff"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min[colnums[week_num]],params$sd_S)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min[colnums[week_num]],params$sd_S)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min[colnums[week_num]],params$sd_S)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min[colnums[week_num]],params$sd_S)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(20,40)){
      #set initial conditions
      gloeo_prev <- IC
      Schmidt_prev <- IC_S
      #temp model
      Schmidt = rnorm(Nmc,week_min[colnums[week_num]],params$sd_S)
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*(Schmidt-Schmidt_prev)
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }
  
  if(model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2 + params$beta5*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2 + params$beta5*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2 + params$beta5*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2 + params$beta5*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(20,40)){
      #set initial conditions
      gloeo_prev <- IC
      Schmidt_prev <- IC_S
      #temp model
      Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
      SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2 + params$beta5*(Schmidt-Schmidt_prev)
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }
  
  if(model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        if(j == 1){Schmidt_prev <- IC_S}
        #temp model
        Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
        Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
        Schmidt_prev <- Schmidt
      }}
    
    if(week_num %in% c(20,40)){
      #set initial conditions
      #set initial conditions
      gloeo_prev <- IC
      Schmidt_prev <- IC_S
      #temp model
      Schmidt = rnorm(Nmc,week_min_S[colnums[week_num]],params$sd_S)
      Wnd = rnorm(Nmc,week_min_W[colnums[week_num]],params$sd_W)
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd + params$beta4*(Schmidt-Schmidt_prev)
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }
  
  if(model_name == "Seasonal_AR_Mintemp_Lag"){
    
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
        Temp = rnorm(Nmc,week_min[j-1],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_SWradiation_Quad"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
      #set initial conditions
      gloeo_prev <- IC
      #temp model
      SW = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_SW)
      #process model
      gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*SW + params$beta4*SW^2
      proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
      #data model
      out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
    }
  }
  
  if(model_name == "Seasonal_DayLength_Quad"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
        #set initial conditions
        gloeo_prev <- IC
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2
        proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
      }
  }
  
  if(model_name == "Seasonal_DayLength_Quad_Mintemp"){
    if(week_num %in% c(1:16,21:36)){
      for(j in 1:5){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(17,37)){
      for(j in 1:4){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(18,38)){
      for(j in 1:3){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(19,39)){
      for(j in 1:2){
        #set initial conditions
        if(j == 1){gloeo_prev <- IC}
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
        proc.model[,j] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,j] = rnorm(Nmc,proc.model[,j],params$sd_obs)
        #update IC
        gloeo_prev <- out[,j]
      }}
    
    if(week_num %in% c(20,40)){
        #set initial conditions
        gloeo_prev <- IC
        #temp model
        DayLength = rnorm(Nmc,week_avg[colnums[week_num]],params$sd_D)
        #temp model
        Temp = rnorm(Nmc,week_min[colnums[week_num]],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*DayLength + params$beta4*DayLength^2 + params$beta5*Temp
        proc.model[,1] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,1] = rnorm(Nmc,proc.model[,1],params$sd_obs)
      }
  }
  
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
  
  if(model_name == "Seasonal_AR_MaxSchmidt_Lag"){
    
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
        Schmidt = rnorm(Nmc,week_max[j-1],params$sd_S)
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
      Schmidt_prev = rnorm(Nmc,week_avg[i],params$sd_S)
      
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
  
  if(model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag"){
    
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
        Schmidt = rnorm(Nmc,week_max[j-1],params$sd_S)
        Temp = rnorm(Nmc,week_min[j-1],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp + params$beta4*Schmidt
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC
      }}}
  
  if(model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight"){
    
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
        Light = rnorm(Nmc,week_avg[j],params$sd_L)
        Temp = rnorm(Nmc,week_min[j-1],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp + params$beta4*Light
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC
      }}}
  
  if(model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag"){
    
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
        Wnd = rnorm(Nmc,week_avg[j-1],params$sd_W)
        Temp = rnorm(Nmc,week_min[j-1],params$sd_T)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Temp + params$beta4*Wnd
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC
      }}}
  
  if(model_name == "Seasonal_AR_Ppt"){
    
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
        Ppt = rnorm(Nmc,week_avg[j],params$sd_P)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Ppt
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR_PAR"){
    
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
        Light = rnorm(Nmc,week_avg[j],params$sd_L)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Light
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR_UnderwaterLight"){
    
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
        Light = rnorm(Nmc,week_avg[j],params$sd_L)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Light
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  if(model_name == "Seasonal_AR_Wnd"){
    
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
        Wnd = rnorm(Nmc,week_avg[j],params$sd_W)
        #process model
        gloeo_temp = params$beta1 + params$beta2*gloeo_prev + params$beta3*Wnd
        proc.model[,t[j]] = rnorm(Nmc,gloeo_temp,params$sd_proc)
        #data model
        out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$sd_obs)
        #update IC
        gloeo_prev <- out[,t[j]] # update IC 
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
  N.cols <- c("gray","coral","deepskyblue4","gold","darkseagreen3")
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
    if(is.forecast.ci == "y"){ciEnvelope(forecast_times[qs[l,]],forecast.ci[1,qs[l,]],forecast.ci[3,qs[l,]],col = "lightblue")}
    lines(forecast_times[qs[l,]], exp(det.prediction[qs[l,]]), col="black", lwd=5)
    points(forecast_times[qs[l,]],forecast_ys[qs[l,]],pch="+",cex=3)
    legend("topleft",legend = forecast_years[l], bty = "n", cex = 3)

  }}
  
  #title(main="Obs (+), Latent CI (blue), PI (green), Obs PI (grey), Mean Pred. (<>)",outer=T, cex = 3) 
  
}

make_varMat <- function(model_name){
  
  if(model_name == "Seasonal_RandomWalk" | model_name == "Seasonal_RandomWalk_Obs_error"){
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    var.IC.P.O   <- apply(vardat.IC.P.O,2,var)
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
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    var.IC.P.O   <- apply(vardat.IC.P.O,2,var)
    var.IC.P.O.Pa   <- apply(vardat.IC.P.O.Pa,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.O,var.IC.P.O.Pa)
  }
  
  if(model_name == "Seasonal_AR_Temperature" | model_name == "Seasonal_AR_Temp_and_Diff" | model_name == "Seasonal_AR_Schmidt" | model_name == "Seasonal_AR_Schmidt_and_Diff" | model_name == "Seasonal_AR_Ppt" | model_name == "Seasonal_AR_PAR" | model_name == "Seasonal_AR_Wnd" |  model_name == "Seasonal_AR_Mintemp" | model_name == "Seasonal_AR_Mintemp_Lag" | model_name == "Seasonal_AR_MaxSchmidt_Lag" | model_name == "Seasonal_AR_MinSchmidt_Diff" | model_name == "Seasonal_GDD_Quad" | model_name == "Seasonal_DayLength_Quad" | model_name == "Seasonal_AR_UnderwaterLight" | model_name == "Seasonal_AR_Mintemp_Lag_MaxSchmidt_Lag" | model_name == "Seasonal_AR_Mintemp_Lag_UnderwaterLight" | model_name == "Seasonal_AR_Mintemp_Lag_Wnd90_Lag" | model_name == "Seasonal_DayLength_Quad_Mintemp" | model_name == "Seasonal_AR_SWradiation_MinSchmidt_Diff" | model_name == "Seasonal_AR_Minwind_MinSchmidt_Diff" | model_name == "Seasonal_SWradiation_Quad"){
    var.IC     <- apply(vardat.IC,2,var)
    var.IC.P    <- apply(vardat.IC.P,2,var)
    var.IC.P.O   <- apply(vardat.IC.P.O,2,var)
    var.IC.P.O.Pa   <- apply(vardat.IC.P.O.Pa,2,var)
    var.IC.P.O.Pa.D   <- apply(vardat.IC.P.O.Pa.D,2,var)
    vm <- rbind(var.IC,var.IC.P,var.IC.P.O,var.IC.P.O.Pa,var.IC.P.O.Pa.D)
  }
  
  return(vm)
  
}

plot_varMat <- function(model_name){
  
  N.cols <- c("gray","coral","deepskyblue4","gold","darkseagreen3","orange")  
  V.pred.rel.2015 <- apply(varMat1[,1:20],2,function(x) {x/max(x)})
  V.pred.rel.2016 <- apply(varMat1[,21:40],2,function(x) {x/max(x)})

  if(nrow(V.pred.rel.2015) == 3){
    
    par(mfrow = c(1,2))
    
    plot(forecast_times[1:20], V.pred.rel.2015[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2015")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel.2015)), V.pred.rel.2015[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[1,], V.pred.rel.2015[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[2,], V.pred.rel.2015[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[3,], rep(1,20), col = N.cols[3])
    legend("bottomright", legend=c("Initial Cond","Process","Observation"), col=N.cols[1:3], lty=1, lwd=3, bg = 'white', cex = 0.8)
    
    plot(forecast_times[21:40], V.pred.rel.2016[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2016")
    ciEnvelope(forecast_times[21:40], rep(0,ncol(V.pred.rel.2016)), V.pred.rel.2016[1,], col = N.cols[1])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[1,], V.pred.rel.2016[2,], col = N.cols[2])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[2,], V.pred.rel.2016[3,], col = N.cols[3])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[3,], rep(1,20), col = N.cols[3])
    legend("bottomright", legend=c("Initial Cond","Process","Observation"), col=N.cols[1:3], lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  else if(nrow(V.pred.rel.2015) == 5){
    par(mfrow = c(1,2))
    plot(forecast_times[1:20], V.pred.rel.2015[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2015")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel.2015)), V.pred.rel.2015[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[1,], V.pred.rel.2015[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[2,], V.pred.rel.2015[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[3,], V.pred.rel.2015[4,], col = N.cols[4])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[4,], V.pred.rel.2015[5,], col = N.cols[5])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[5,], rep(1,20), col = N.cols[5])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Parameter","Driver"), col=N.cols[1:5], lty=1, lwd=3, bg = 'white', cex = 0.8)
    
    plot(forecast_times[21:40], V.pred.rel.2016[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2016")
    ciEnvelope(forecast_times[21:40], rep(0,ncol(V.pred.rel.2016)), V.pred.rel.2016[1,], col = N.cols[1])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[1,], V.pred.rel.2016[2,], col = N.cols[2])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[2,], V.pred.rel.2016[3,], col = N.cols[3])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[3,], V.pred.rel.2016[4,], col = N.cols[4])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[4,], V.pred.rel.2016[5,], col = N.cols[5])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[5,], rep(1,20), col = N.cols[5])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Parameter","Driver"), col=N.cols[1:5], lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  else if(nrow(varMat) == 4 & model_name == "Seasonal_RandomWalk_RandomYear"){
    par(mfrow = c(1,2))
    
    plot(forecast_times[1:20], V.pred.rel.2015[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2015")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel.2015)), V.pred.rel.2015[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[1,], V.pred.rel.2015[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[2,], V.pred.rel.2015[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[3,], V.pred.rel.2015[4,], col = N.cols[6])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[4,], rep(1,20), col = N.cols[6])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Random Effects"), col=c(N.cols[1:3],N.cols[6]), lty=1, lwd=3, bg = 'white', cex = 0.8)
    
    plot(forecast_times[21:40], V.pred.rel.2016[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2016")
    ciEnvelope(forecast_times[21:40], rep(0,ncol(V.pred.rel.2016)), V.pred.rel.2016[1,], col = N.cols[1])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[1,], V.pred.rel.2016[2,], col = N.cols[2])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[2,], V.pred.rel.2016[3,], col = N.cols[3])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[3,], V.pred.rel.2016[4,], col = N.cols[6])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[4,], rep(1,20), col = N.cols[6])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Random Effects"), col=c(N.cols[1:3],N.cols[6]), lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
  else {
    par(mfrow = c(1,2))
    plot(forecast_times[1:20], V.pred.rel.2015[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2015")
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel.2015)), V.pred.rel.2015[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[1,], V.pred.rel.2015[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[2,], V.pred.rel.2015[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[3,], V.pred.rel.2015[4,], col = N.cols[4])
    ciEnvelope(forecast_times[1:20], V.pred.rel.2015[4,], rep(1,20), col = N.cols[4])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Parameter"), col=N.cols[1:4], lty=1, lwd=3, bg = 'white', cex = 0.8)
    
    plot(forecast_times[21:40], V.pred.rel.2016[1,], ylim=c(0,1), type='n', main="Relative Variance", ylab="Proportion of Variance", xlab="Sampling season 2016")
    ciEnvelope(forecast_times[21:40], rep(0,ncol(V.pred.rel.2016)), V.pred.rel.2016[1,], col = N.cols[1])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[1,], V.pred.rel.2016[2,], col = N.cols[2])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[2,], V.pred.rel.2016[3,], col = N.cols[3])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[3,], V.pred.rel.2016[4,], col = N.cols[4])
    ciEnvelope(forecast_times[21:40], V.pred.rel.2016[4,], rep(1,20), col = N.cols[4])
    legend("bottomright", legend=c("Initial Cond","Process","Observation","Parameter"), col=N.cols[1:4], lty=1, lwd=3, bg = 'white', cex = 0.8)
  }
  
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

# ##forecast plot on log scale
# lims <- c(min(forecast.ci.IC.P.O.Pa.D[1,c(1:20)])-0.2, max(forecast.ci.IC.P.O.Pa.D[3,c(1:20)])+0.2)
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/forecast_log.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #ic only
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_IC.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #ic + p
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_ICP.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #ic + p + o
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_ICPO.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #ic + p + o + pa
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_ICPOPa.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa[1,c(1:20)],forecast.ci.IC.P.O.Pa[3,c(1:20)],col="gold")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #ic + p + o + pa + d
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_ICPOPaD.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa.D[1,c(1:20)],forecast.ci.IC.P.O.Pa.D[3,c(1:20)],col="darkseagreen3")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa[1,c(1:20)],forecast.ci.IC.P.O.Pa[3,c(1:20)],col="gold")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #not partitioned
# #ic + p + o + pa + d
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_nopart.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa.D[1,c(1:20)],forecast.ci.IC.P.O.Pa.D[3,c(1:20)],col="lightblue")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# dev.off()
# 
# #ic + p + o + pa + d + obs
# png(filename = "C:/Users/Mary Lofton/Documents/Ch5/GLEON_poster_results/Uncertainty_partitioning/ci_log_ICPOPaD_obs.png",
#     width = 8, height = 6, units = "in", res = 300)
# par(mgp = c(2.3,1,0))
# plot(forecast_times[1:20],log(forecast_ys[1:20]),type = "n",
#      xlab = "",ylab = expression(paste("log(colonies", ~~L^{-1},")")),cex.lab = 1.5, cex.axis = 1.4,
#      ylim = lims)
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa.D[1,c(1:20)],forecast.ci.IC.P.O.Pa.D[3,c(1:20)],col="darkseagreen3")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O.Pa[1,c(1:20)],forecast.ci.IC.P.O.Pa[3,c(1:20)],col="gold")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P.O[1,c(1:20)],forecast.ci.IC.P.O[3,c(1:20)],col="deepskyblue4")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC.P[1,c(1:20)],forecast.ci.IC.P[3,c(1:20)],col="coral")
# ciEnvelope(forecast_times[1:20],forecast.ci.IC[1,c(1:20)],forecast.ci.IC[3,c(1:20)],col="gray")
# lines(forecast_times[1:20],det.prediction[1:20], lwd = 2)
# points(forecast_times[1:20],forecast_ys[1:20],pch = 16)
# dev.off()
