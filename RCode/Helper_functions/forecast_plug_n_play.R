#Forecast plug n play
#Author:Mary Lofton
#Date: 05OCT19

forecast_gloeo <- function(model_name, params, settings){
  
  IC = settings$IC
  N_out = settings$N_out
  Nmc = settings$Nmc
  proc.model <- matrix(NA, Nmc, N_out) # setting up output 
  out <- matrix(NA, Nmc, N_out)
  ts = rbind(1:20,21:40)
  forecast_years = forecast_years
  
  if(model_name == "Seasonal_RandomWalk"){
    
  for(k in 1:length(forecast_years)){
    gloeo_prev <- IC[,k]
    t <- ts[k,]
    for(j in 2:max(season_weeks)){
      #populate first week of season with IC
      proc.model[,t[j-1]] <- IC[,k]
      out[,t[j-1]] <- IC[,k]
      #process model
      proc.model[,t[j]] = rnorm(Nmc,gloeo_prev,params$tau_proc)
      #data model
      out[,t[j]] = rnorm(Nmc,proc.model[,t[j]],params$tau_obs)
      #update IC
      gloeo_prev <- out[,t[j]] # update IC 
      }}}
  
  return(out)
}

forecast_plot <- function(cal_years, forecast_years, is.forecast.ci){
  
  rows <- (length(cal_years) + length(forecast_years))/2
  ps = rbind(1:20,21:40,41:60,61:80,81:100,101:120)
  qs = rbind(1:20,21:40)
  ci = ci
  pi = pi
  obs_pi = obs_pi
  
  par(mfrow = c(rows,2), oma = c(1,1,5,1), mar = c(3,3,1,0)+0.1,
      mgp = c(2,0.5,0))
  
  for (i in 1:length(cal_years)){
    
  plot(times[ps[i,]],ci[2,ps[i,]],type='n', ylab="Gloeo (tot./L)", ylim = c(min(obs_pi[1,ps[i,]], na.rm = TRUE),max(obs_pi[3,ps[i,]], na.rm = TRUE)),
       main="",xlab = "")
  ciEnvelope(times[ps[i,]],obs_pi[1,ps[i,]],obs_pi[3,ps[i,]],col="gray")
  ciEnvelope(times[ps[i,]],pi[1,ps[i,]],pi[3,ps[i,]],col="Green")
  ciEnvelope(times[ps[i,]],ci[1,ps[i,]],ci[3,ps[i,]],col="lightBlue")
  points(times[ps[i,]],ys[ps[i,]],pch="+",cex=0.8)
  legend("topleft",legend = cal_years[i], bty = "n")

  }
  
  if(!is.na(forecast_years[1])){
  
  for (l in 1:length(forecast_years)){
    
    if(is.forecast.ci == "y"){
      lims <- c(min(forecast.ci[1,qs[l,]], na.rm = TRUE),max(forecast.ci[3,qs[l,]], na.rm = TRUE))} 
    else {lims <- range(forecast_ys, na.rm = TRUE)}
    
    plot(forecast_times[qs[l,]],forecast_ys[qs[l,]],type='n', ylab="Gloeo (tot./L)", ylim = lims,
         main="",xlab = "")
    lines(forecast_times[qs[l,]], exp(det.prediction[qs[l,]]), col="purple", lwd=2)
    points(forecast_times[qs[l,]],forecast_ys[qs[l,]],pch="+",cex=0.8)
    legend("topleft",legend = cal_years[l], bty = "n")

  }}
  
  title(main="Obs (+), Latent CI (blue), PI (green), Obs PI (grey), Mean Pred. (<>)",outer=T) 
  
}
