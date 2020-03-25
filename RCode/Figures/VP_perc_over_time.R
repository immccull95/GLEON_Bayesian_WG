#Title: figure of relative contributions of uncertainty over time
#Author: MEL
#Date: 17FEB2020

pacman::p_load(tidyverse,lubridate)

#set up model names
model_names <- c("Seasonal_RandomWalk","Seasonal_RandomWalk_Obs_error",
                 "Seasonal_AR","Seasonal_AR_MinSchmidt_Diff",
                 "Seasonal_SWradiation_Quad","Seasonal_AR_Minwind_MinSchmidt_Diff",
                 "Seasonal_AR_SWradiation_MinSchmidt_Diff")

#read in data
forecast_y <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv") %>%
  filter(site == "midge")

forecast_dat <- dat %>% filter(year(date) %in% c(2016))
forecast_times <- as.Date(as.character(forecast_dat$date))

#set up ciEnvelope function
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}


#########################1 week ahead partitions

plot_varMat <- function(V.pred.rel, forecast_times, year){
  
  N.cols <- c("gray","coral","deepskyblue4","gold","darkseagreen3","orange")  
  
  if(nrow(V.pred.rel) == 3){
    
    layout(rbind(1,2), heights=c(7,1))
    par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", xlab="", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1)
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], rep(1,20), col = N.cols[3])
    legend("bottomright",legend = year,bty = "n", cex = 1.5)
    abline(v = forecast_times[3], lwd = 3)
    abline(v = forecast_times[12], lwd = 3)
    par(mar=c(0, 2.1, 0, 0))
    plot.new()
    legend("center", legend=c("Initial Cond","Process","Observation"), col=N.cols[1:3], lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 3, bty = "n", seg.len = 0.5)
    
  }
  
  else if(nrow(V.pred.rel) == 5){
    layout(rbind(1,2), heights=c(7,1))
    par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", xlab="", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1)
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[4])
    ciEnvelope(forecast_times[1:20], V.pred.rel[4,], V.pred.rel[5,], col = N.cols[5])
    ciEnvelope(forecast_times[1:20], V.pred.rel[5,], rep(1,20), col = N.cols[5])
    legend("bottomright",legend = year,bty = "n", cex = 1.5)
    abline(v = forecast_times[3], lwd = 3)
    abline(v = forecast_times[12], lwd = 3)
    par(mar=c(0, 2.1, 0, 0))
    plot.new()
    legend("center", legend=c("Initial Cond","Process","Observation","Parameter","Driver"), col=N.cols[1:5], lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 5, bty = "n", seg.len = 0.5)
  }
  
  else {
    layout(rbind(1,2), heights=c(7,1))
    par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
    plot(forecast_times[1:20], V.pred.rel[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", xlab="", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1)
    ciEnvelope(forecast_times[1:20], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[1:20], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[1:20], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[1:20], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[4])
    ciEnvelope(forecast_times[1:20], V.pred.rel[4,], rep(1,20), col = N.cols[4])
    legend("bottomright",legend = year,bty = "n", cex = 1.5)
    abline(v = forecast_times[3], lwd = 3)
    abline(v = forecast_times[12], lwd = 3)
    par(mar=c(0, 2.1, 0, 0))
    plot.new()
    legend("center", legend=c("Initial Cond","Process","Observation","Parameter"), col=N.cols[1:4], lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 4, bty = "n", seg.len = 0.5)
  }
  
}


for (i in 1:length(model_names)){
  
  varMat_2016 <- read_csv(file = file.path("./Results/Uncertainty_partitioning/w_Data_assim_1wk/",paste0("Midge_",model_names[i],"_varMat_2016.csv")))
  varMat_2016 <- apply(varMat_2016,2,sort,decreasing=F)
  
  tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[i],"_V.pred.rel.2016.tif")),
       width = 8, height = 6, units = "in", res = 300)
  plot_varMat(V.pred.rel = varMat_2016,
              forecast_times = forecast_times[1:20],
              year = "2016")
  dev.off()
  
}





#########################4 week ahead partitions


plot_varMat <- function(V.pred.rel, forecast_times, year){
  
  N.cols <- c("gray","coral","deepskyblue4","gold","darkseagreen3","orange")  
  
  if(nrow(V.pred.rel) == 3){
    
    layout(rbind(1,2), heights=c(7,1))
    par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
    plot(forecast_times[4:19], V.pred.rel[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", xlab="", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1)
    ciEnvelope(forecast_times[4:19], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[4:19], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[4:19], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[4:19], V.pred.rel[3,], rep(1,20), col = N.cols[3])
    legend("bottomright",legend = year,bty = "n", cex = 1.5)
    abline(v = forecast_times[3], lwd = 3)
    abline(v = forecast_times[12], lwd = 3)
    par(mar=c(0, 2.1, 0, 0))
    plot.new()
    legend("center", legend=c("Initial Cond","Process","Observation"), col=N.cols[1:3], lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 3, bty = "n", seg.len = 0.5)
    
  }
  
  else if(nrow(V.pred.rel) == 5){
    layout(rbind(1,2), heights=c(7,1))
    par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
    plot(forecast_times[4:19], V.pred.rel[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", xlab="", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1)
    ciEnvelope(forecast_times[4:19], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[4:19], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[4:19], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[4:19], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[4])
    ciEnvelope(forecast_times[4:19], V.pred.rel[4,], V.pred.rel[5,], col = N.cols[5])
    ciEnvelope(forecast_times[4:19], V.pred.rel[5,], rep(1,20), col = N.cols[5])
    legend("bottomright",legend = year,bty = "n", cex = 1.5)
    abline(v = forecast_times[3], lwd = 3)
    abline(v = forecast_times[12], lwd = 3)
    par(mar=c(0, 2.1, 0, 0))
    plot.new()
    legend("center", legend=c("Initial Cond","Process","Observation","Parameter","Driver"), col=N.cols[1:5], lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 5, bty = "n", seg.len = 0.5)
  }
  
  else {
    layout(rbind(1,2), heights=c(7,1))
    par(mar=c(2, 4.1, 1, 1), mgp = c(3,1,0))
    plot(forecast_times[4:19], V.pred.rel[1,], ylim=c(0,1), type='n', main="", ylab="Proportion of Variance", xlab="", cex.lab = 1.5, cex.axis = 1.5, xaxs="i", yaxs="i", las = 1)
    ciEnvelope(forecast_times[4:19], rep(0,ncol(V.pred.rel)), V.pred.rel[1,], col = N.cols[1])
    ciEnvelope(forecast_times[4:19], V.pred.rel[1,], V.pred.rel[2,], col = N.cols[2])
    ciEnvelope(forecast_times[4:19], V.pred.rel[2,], V.pred.rel[3,], col = N.cols[3])
    ciEnvelope(forecast_times[4:19], V.pred.rel[3,], V.pred.rel[4,], col = N.cols[4])
    ciEnvelope(forecast_times[4:19], V.pred.rel[4,], rep(1,20), col = N.cols[4])
    legend("bottomright",legend = year,bty = "n", cex = 1.5)
    abline(v = forecast_times[3], lwd = 3)
    abline(v = forecast_times[12], lwd = 3)
    par(mar=c(0, 2.1, 0, 0))
    plot.new()
    legend("center", legend=c("Initial Cond","Process","Observation","Parameter"), col=N.cols[1:4], lty=1, lwd=10, bg = 'white', cex = 1.2, ncol = 4, bty = "n", seg.len = 0.5)
  }
  
}

for (i in 1:length(model_names)){
  
  varMat_2016 <- read_csv(file = file.path("./Results/Uncertainty_partitioning/w_Data_assim_4wk/",paste0("Midge_",model_names[i],"_varMat_2016.csv")))
  varMat_2016 <- varMat_2016[,1:16]
  varMat_2016 <- apply(varMat_2016,2,sort,decreasing=F)
  
  tiff(file = file.path("C:/Users/Mary Lofton/Dropbox/Ch5/Final_figs/",paste0(model_names[i],"_V.pred.rel.2016_4wk.tif")),
       width = 8, height = 6, units = "in", res = 300)
  plot_varMat(V.pred.rel = varMat_2016,
              forecast_times = forecast_times[1:20],
              year = "2016")
  dev.off()
  
}

