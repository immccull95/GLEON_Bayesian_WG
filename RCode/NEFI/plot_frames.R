
# generating pngs 
t_col <- function(color, percent = 50, name = NULL) {
  #	  color = color name
  #	percent = % transparency
  #	   name = an optional name for the color
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
  
}

for(q in 1:length(forecast_times)){
  # cur <- eval(parse(text = paste0('jags.out',q)))
  trunc_date <- grep(forecast_times[q], all_data$date)
  
  if(q == 1){
    cur <- eval(parse(text = paste0('ci_',q)))
    ci = cur
  }else{ # truncating to previous forecast 
    # previous <- eval(parse(text = paste0('ci_',(q-1)))) # previous forecast 
    # previous <- previous[,1:(trunc_date-1)]
    ci_previous <- ci_previous[,1:(trunc_date-1)]
    cur <- eval(parse(text = paste0('ci_',q)))
    cur <- cur[,trunc_date:ncol(cur)]
    cur <- cbind(ci_previous, cur)
    ci = cur
  }

  # cur <- as.matrix(cur)
  # x.cols <- grep("^m",colnames(cur)) ## grab all columns that start with the letter m
  # ci <- apply(cur[,x.cols],2,quantile,c(0.025,0.5,0.975))
  
  # med = ci[2,]
  # med = med[!is.infinite(med)]
  ylim = range(data$y,na.rm = T)
  
  # time.rng = c(1,ncol(ci)) ## adjust to zoom in and out
  # time = seq(time.rng[1], time.rng[2])
  date = all_data$date
  
  cex.lab = 1.5
  cex.axis = 1.5 
  lwd = 3
  
  png(filename = paste0('Figs/NEFI/tmp/time_',q, '.png'), width = 12, height = 7, units = 'in', res= 100)
  ylim=ylim
  plot(date,ci[2,],ylab="Gloeo Colonies", xlab = '', type='l', lwd= lwd, col = t_col('black', percent = 0),cex.lab = cex.lab,
       cex.axis=cex.axis, log = 'y')
  ecoforecastR::ciEnvelope(date,ci[1,],ci[3,],col=t_col('blue', percent = 60))
  lines(date,ci[2,], lwd =lwd, col = t_col('black', percent = 0))
  points(date,data$y,pch=16,cex=2, col= t_col('red', percent = 60))
  # lines(date,data$y[i]+c(-1,1)*LAIr.sd[i])
  # add bar to forecast time 
  legend('topleft', legend = c('Obs', 'Predict'), col = c('red','black'), lwd = c(NA, 3), pch = c(16, NA), lty = c(NA, 1),bg = 'white')
  mtext('Forecast =>', side=1, line=2.5, cex = cex.lab, adj = 0, col = 'blue', at=forecast_times[q])
  
  dev.off() 
  
  ci_previous <- ci 
  
  print(c(q, "of", length(forecast_times)))
}




