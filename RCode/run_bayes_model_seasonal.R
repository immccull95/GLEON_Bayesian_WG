# State-space model - revised by S. LaDeau (11/2017) from the EcoForecast Activity by Michael Dietze, with reference "Ecological Forecasting", chapter 8
# ========================================================
#   
#   The data used for this example are from summer weekly(ish) Gloetrichia echinulata (Gloeo.) sampling at 4 locations in Lake Sunapee, NH. The data are provided by Kathryn Cottingham, and should not be used without permission outside this workshop.

library(tidyverse)
library(readxl)
library(rjags)
library(runjags)
library(moments)
library(geosphere)
library(googledrive)
source('RCode/helper_functions/seasonal_plug_n_play.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

model_name = 'Seasonal_SWradiation_Quad_Mintemp' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
model=paste0("RCode/Jags_Models/Seasonal_for_loop/",model_name, '.R') #Do not edit

#How many times do you want to sample to get predictive interval for each sampling day?
#Edit nsamp to reflect a subset of total number of samples
nsamp = 1500 

#My local directory - use as a temporary file repository for plot files before uploading
#to Google Drive for the team to see :)
my_directory <- "C:/Users/Mary Lofton/Dropbox/Ch5/Final_analysis"


#2) read in and visualize data ------------------------------------------------------------------------------------------------------------
y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_22JUL19.csv"))+0.003)


#for GDD
GDD <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_28JAN20.csv"))
GDD <- scale(GDD, center = TRUE, scale = TRUE)

#for DayLength
DayLength <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/daylength_year_by_week_28JAN20.csv"))
DayLength <- scale(DayLength, center = TRUE, scale = TRUE)

#for SW radiation
SW <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_24FEB20.csv"))
SW <- scale(SW, center = TRUE, scale = TRUE)

#for Minwind
Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_minwind_24FEB20.csv"))
Wnd <- scale(Wnd, center = TRUE, scale = TRUE)

#for CVwind
Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_CVwind_24FEB20.csv"))
Wnd <- scale(Wnd, center = TRUE, scale = TRUE)

#for watertemp_min
Temp <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_16AUG19.csv"))
Temp <- scale(Temp, center = TRUE, scale = TRUE)
Temp_prior <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Fichter_year_by_week_watertemp_min_16AUG19.csv"))
Temp_prior <- scale(Temp_prior, center = TRUE, scale = TRUE)

#for max Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_28JAN20.csv"))
Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)

#for min Schmidt
Schmidt <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_min_Schmidt_28JAN20.csv"))
Schmidt <- scale(Schmidt, center = TRUE, scale = TRUE)

#for underwater light from HOBOs
Light <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/UnderwaterLight_year_by_week_02FEB20.csv"))
Light <- scale(Light, center = TRUE, scale = TRUE)

#for Ppt
Ppt <- read_csv("C:/Users/Mary Lofton/Documents/RProjects/GLEON_Bayesian_WG/Datasets/Sunapee/Bayes_Covariates_Data/midge_weekly_summed_precip_10OCT19.csv")

years <- c(2009:2014)
year_no = as.numeric(as.factor(years))
season_weeks = c(1:20)
site = "Midge"

#for water temp
week_avg = colMeans(Temp_prior, na.rm = TRUE)

#for min water temp
week_min = colMeans(Temp_prior, na.rm = TRUE)
# week_var_mean = mean(1/apply(Temp_prior,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(Temp_prior,2,var),na.rm = TRUE)

#for Schmidt
week_avg = colMeans(Schmidt, na.rm = TRUE)
# week_var_mean = mean(1/apply(Schmidt,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(Schmidt,2,var),na.rm = TRUE)

#for max Schmidt
week_max = colMeans(Schmidt, na.rm = TRUE)
# week_var_mean = mean(1/apply(Schmidt,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(Schmidt,2,var),na.rm = TRUE)

#for min Schmidt
week_min = colMeans(Schmidt, na.rm = TRUE)
# week_var_mean = mean(1/apply(Schmidt,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(Schmidt,2,var),na.rm = TRUE)

#for GDD
week_avg = colMeans(GDD, na.rm = TRUE)
#week_var_mean = mean(1/apply(GDD,2,var),na.rm = TRUE)
#week_var_var = var(1/apply(GDD,2,var),na.rm = TRUE)

#for DayLength
week_avg = colMeans(DayLength, na.rm = TRUE)
# week_var_mean = mean(1/apply(DayLength,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(DayLength,2,var),na.rm = TRUE)

#for SW radiation
week_avg = colMeans(SW, na.rm = TRUE)

#for precipitation
week_avg = colMeans(Ppt, na.rm = TRUE)
# week_var_mean = mean(1/apply(Ppt,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(Ppt,2,var),na.rm = TRUE)

#for underwater light
week_avg = colMeans(Light, na.rm = TRUE)
week_avg[c(19,20)]<- week_avg[18]
# week_var_mean = mean(1/apply(Light,2,var),na.rm = TRUE)
# week_var_var = var(1/apply(Light,2,var),na.rm = TRUE)

#for Minwind
week_min = colMeans(Wnd, na.rm = TRUE)

#for CVwind
week_cv = colMeans(Wnd, na.rm = TRUE)

#for combined covariate model
week_avg_T = colMeans(Temp_prior, na.rm = TRUE)
week_avg_S = colMeans(Schmidt, na.rm = TRUE)

#for combined covariate model
week_min_T = colMeans(Temp_prior, na.rm = TRUE)
week_min_S = colMeans(Schmidt, na.rm = TRUE)
week_min_W = colMeans(Wnd, na.rm = TRUE)


#3) JAGS Plug-Ins -----------------------------------------------------------------------------------------------------
jags_plug_ins <- jags_plug_ins(model_name = model_name)

# Now that we've defined the model, the data, and the initialization, we need to send all this info to JAGS, which will return the JAGS model object.

#4) Run model (no edits, unless you want to change # of iterations) -------------------------------------------------------------
j.model   <- jags.model (file = model,
                         data = jags_plug_ins$data.model,
                         inits = jags_plug_ins$init.model,
                         n.chains = 3)

jags.out <- run.jags(model = model,
                     data = jags_plug_ins$data.model,
                     adapt =  5000, 
                     burnin =  100000, 
                     sample = 5000, 
                     n.chains = 3, 
                     inits=jags_plug_ins$init.model,
                     monitor = jags_plug_ins$variable.namesout.model)

#5) Save and Process Output
write.jagsfile(jags.out, file=file.path("Results/Jags_Models/Final_analysis",paste(site,paste0(model_name,'.txt'), sep = '_')), 
               remove.tags = TRUE, write.data = TRUE, write.inits = TRUE)

#this will create multiple plots if var names are different but doesn't create multiple
#plots for vars with same names, i.e., betas, so have created a quick hack to fix that

params <- jags_plug_ins$params.model

for (i in 1:length(params)){
  png(file=file.path(my_directory,paste(site,paste0(model_name,'_Convergence_',params[i],'.png'), sep = '_')))
  plot(jags.out, vars = params[i]) 
  dev.off()
}


#need to view this to get parameter estimates for model comparison Excel file
sum <- summary(jags.out, vars = jags_plug_ins$variable.names.model)
DIC=dic.samples(j.model, n.iter=5000)

#save results
sink(file = file.path("Results/Jags_Models/Final_analysis",paste(site,paste0(model_name,'_param_summary.txt'), sep = '_')))
print("Parameter summary")
sum
print("DIC")
DIC
sink()

jags.out.mcmc <- as.mcmc.list(jags.out)
out <- as.matrix(jags.out.mcmc)

#Seasonal_RandomWalk_Obs_error
saveRDS(object = DIC, file = file.path("Results/Jags_Models/Final_analysis", paste(site, paste0(model_name,'_DIC.rds'), sep = '_')))


#6) CI, PI, Obs PI Calculations

dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp.csv") %>%
  filter(site == "midge")

times <- as.Date(as.character(dat$date))
#time.rng = c(1,20) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

mus=c(grep("mu\\[1,", colnames(out)),grep("mu\\[2,", colnames(out)),
      grep("mu\\[3,", colnames(out)),grep("mu\\[4,", colnames(out)),
      grep("mu\\[5,", colnames(out)),grep("mu\\[6,", colnames(out)))
mu = out[,mus]
ci <- exp(apply(mu,2,quantile,c(0.025,0.5,0.975)))


## One step ahead prediction intervals

samp <- sample.int(nrow(out),nsamp)
mu = out[samp,mus] 
Temps=c(Temp[1,], Temp[2,], Temp[3,], Temp[4,], Temp[5,], Temp[6,])
Schmidts=c(Schmidt[1,], Schmidt[2,], Schmidt[3,], Schmidt[4,], Schmidt[5,], Schmidt[6,])
Ppts=c(Ppt[1,], Ppt[2,], Ppt[3,], Ppt[4,], Ppt[5,], Ppt[6,])
Lights=c(Light[1,], Light[2,], Light[3,], Light[4,], Light[5,], Light[6,])
Wnds=c(Wnd[1,], Wnd[2,], Wnd[3,], Wnd[4,], Wnd[5,], Wnd[6,])
GDDs=c(GDD[1,], GDD[2,], GDD[3,], GDD[4,], GDD[5,], GDD[6,])
DayLengths=c(DayLength[1,], DayLength[2,], DayLength[3,], DayLength[4,], DayLength[5,], DayLength[6,])
SWs=c(SW[1,], SW[2,], SW[3,], SW[4,], SW[5,], SW[6,])

ys = exp(c(y[1,],y[2,],y[3,],y[4,],y[5,],y[6,]))


#get one-step-ahead predictions
preds_plug_ins <- preds_plug_ins(model_name) 

pi <- exp(apply(preds_plug_ins$pred.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))
obs_pi <- exp(apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))


#7) CI, PI, Obs PI Plots


#CI, PI, Obs PI
png(file=file.path(my_directory,paste(site,paste0(model_name,'_CI_PI.png'), sep = '_')), res=300, width=20, height=15, units='cm')
par(mfrow = c(3,2), oma = c(1,1,5,1), mar = c(4,4,2,2)+0.1)

#2009
plot(times[1:20],ci[2,1:20],type='n', ylab="Gloeo density (total per L)", ylim = c(min(obs_pi[1,1:20], na.rm = TRUE),max(obs_pi[3,1:20], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[1:20],obs_pi[1,1:20],obs_pi[3,1:20],col="gray")
ciEnvelope(times[1:20],pi[1,1:20],pi[3,1:20],col="Green")
ciEnvelope(times[1:20],ci[1,1:20],ci[3,1:20],col="lightBlue")
points(times[1:20],ys[1:20],pch="+",cex=0.8)
legend("topleft",legend = "2009", bty = "n")
points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2010
plot(times[21:40],ci[2,21:40],type='n', ylab="Gloeo density (total per L)", ylim = c(min(obs_pi[1,21:40], na.rm = TRUE),max(obs_pi[3,21:40], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[21:40],obs_pi[1,21:40],obs_pi[3,21:40],col="gray")
ciEnvelope(times[21:40],pi[1,21:40],pi[3,21:40],col="Green")
ciEnvelope(times[21:40],ci[1,21:40],ci[3,21:40],col="lightBlue")
points(times[21:40],ys[21:40],pch="+",cex=0.8)
legend("topleft",legend = "2010", bty = "n")
points(times[21:40],obs_pi[2,21:40],pch = 5, cex = 0.8)

#2011
plot(times[41:60],ci[2,41:60],type='n', ylab="Gloeo density (total per L)", ylim = c(min(obs_pi[1,41:60],na.rm = TRUE),max(obs_pi[3,41:60],na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[41:60],obs_pi[1,41:60],obs_pi[3,41:60],col="gray")
ciEnvelope(times[41:60],pi[1,41:60],pi[3,41:60],col="Green")
ciEnvelope(times[41:60],ci[1,41:60],ci[3,41:60],col="lightBlue")
points(times[41:60],ys[41:60],pch="+",cex=0.8)
legend("topleft",legend = "2011", bty = "n")
points(times[41:60],obs_pi[2,41:60],pch = 5, cex = 0.8)

#2012
plot(times[61:80],ci[2,61:80],type='n', ylab="Gloeo density (total per L)", ylim = c(min(obs_pi[1,61:80], na.rm = TRUE),max(obs_pi[3,61:80], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[61:80],obs_pi[1,61:80],obs_pi[3,61:80],col="gray")
ciEnvelope(times[61:80],pi[1,61:80],pi[3,61:80],col="Green")
ciEnvelope(times[61:80],ci[1,61:80],ci[3,61:80],col="lightBlue")
points(times[61:80],ys[61:80],pch="+",cex=0.8)
legend("topleft",legend = "2012", bty = "n")
points(times[61:80],obs_pi[2,61:80],pch = 5, cex = 0.8)

#2013
plot(times[81:100],ci[2,81:100],type='n', ylab="Gloeo density (total per L)", ylim = c(min(obs_pi[1,81:100], na.rm = TRUE),max(obs_pi[3,81:100], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[81:100],obs_pi[1,81:100],obs_pi[3,81:100],col="gray")
ciEnvelope(times[81:100],pi[1,81:100],pi[3,81:100],col="Green")
ciEnvelope(times[81:100],ci[1,81:100],ci[3,81:100],col="lightBlue")
points(times[81:100],ys[81:100],pch="+",cex=0.8)
legend("topleft",legend = "2013", bty = "n")
points(times[81:100],obs_pi[2,81:100],pch = 5, cex = 0.8)

#2014
plot(times[101:120],ci[2,101:120],type='n', ylab="Gloeo density (total per L)", ylim = c(min(obs_pi[1,101:120], na.rm = TRUE),max(obs_pi[3,101:120], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[101:120],obs_pi[1,101:120],obs_pi[3,101:120],col="gray")
ciEnvelope(times[101:120],pi[1,101:120],pi[3,101:120],col="Green")
ciEnvelope(times[101:120],ci[1,101:120],ci[3,101:120],col="lightBlue")
points(times[101:120],ys[101:120],pch="+",cex=0.8)
legend("topleft",legend = "2014", bty = "n")
points(times[101:120],obs_pi[2,101:120],pch = 5, cex = 0.8)

title(main="Obs (+), Latent CI (blue), PI (green), Obs PI (grey), Mean Pred. (<>)",outer=T) 


dev.off()

#8) Further Diagnostic Checks and Visualization 

#y vs. preds
ys = c(y[1,],y[2,],y[3,],y[4,],y[5,],y[6,])
obs_diff= vector(mode="numeric", length=0)
obs_quantile = vector(mode="numeric", length=0)
obs_quantile_dm = vector(mode="numeric",length=0)
pred_mean = vector(mode="numeric",length=0)
mypreds <- preds_plug_ins$pred.model[,colSums(is.na(preds_plug_ins$pred.model))<nrow(preds_plug_ins$pred.model)]
mypreds_obs <- preds_plug_ins$pred_obs.model[,colSums(is.na(preds_plug_ins$pred_obs.model))<nrow(preds_plug_ins$pred_obs.model)]
perc_ys <- ys[-c(1,21,41,61,81,101)]
obs_pi1 <- obs_pi[1,-c(1,21,41,61,81,101)]
obs_pi2 <- obs_pi[2,-c(1,21,41,61,81,101)]
obs_pi3 <- obs_pi[3,-c(1,21,41,61,81,101)]



for(i in 2:ncol(mypreds)){
  obs_diff[i]=mean(exp(mypreds_obs[,i]))-exp(perc_ys[i]) #difference between mean of pred. values and obs for each time point
  pred_mean[i]=mean(exp(mypreds[,i])) #mean of pred. values at each time point
  percentile <- ecdf(exp(mypreds[,i])) #create function to give percentile based on distribution of pred. values at each time point
  obs_quantile[i] <- percentile(exp(perc_ys[i])) #get percentile of obs in pred distribution
  percentile1 <- ecdf(exp(mypreds_obs[,i])) #create function to give percentile of obs in distribution of pred including observation error
  obs_quantile_dm[i] <- percentile1(exp(perc_ys[i])) #get percentile of obs 
}

sink(file = file.path("Results/Jags_Models/Final_analysis",paste(site,paste0(model_name,'_obs_pred_differences.txt'), sep = '_')))

#Mean of difference between pred and obs
obspred_mean=mean(obs_diff, na.rm=TRUE)
print("Mean of difference between pred including observation error and obs")
obspred_mean

#Median of difference between pred and obs
obspred_median=median(obs_diff, na.rm=TRUE)
print("Median of difference between pred including observation error and obs")
obspred_median

#Mean quantile of obs in distribution of pred including observation error
obs_quantile_mean_dm = mean(obs_quantile_dm, na.rm = TRUE)
print("Mean quantile of obs in distribution of pred including observation error")
obs_quantile_mean_dm

#Quantile of 2013 bloom point
print("Quantile of 2013 bloom point in distribution of pred including observation error")
obs_quantile_dm[93]

#Mean quantile of observations of 0 Gloeo
zeroes <- unname(exp(perc_ys))
zeroes <- as.numeric(zeroes)
zeroes_check <- which(zeroes == "0.003")
obs_quantile_mean_dm_0 = mean(obs_quantile_dm[zeroes_check], na.rm = TRUE)
print("Mean quantile of observations of 0 Gloeo in distribution of pred including observation error")
obs_quantile_mean_dm_0

#percent of time we are predicting negative Gloeo
print("Percent of time we are predicting negative Gloeo")
length(subset(obs_pi[2,],obs_pi[2,] < 0))/length(obs_pi[2,])*100

#correlation coefficient of pred vs. obs
cor.coef <- cor(exp(perc_ys),obs_pi2, method = "pearson", use = "complete.obs")
print("Pearson's correlation coefficient of observations and 50th quantile of predicted")
cor.coef

#Mean range of 95% predictive interval
mean_range_pred <- mean(obs_pi3-obs_pi1, na.rm = TRUE)
print("Mean range of 95% confidence interval including observation error")
mean_range_pred

sink()


#9) Diagnostic Plots

png(file=file.path(my_directory,paste(site,paste0(model_name,'_Diagnostics.png'), sep = '_')), res=300, width=20, height=15, units='cm')
par(mfrow=c(3,2))

#hist of quantiles
hist(obs_quantile, breaks = seq(0,1,0.05),main="No obs error") #no observation error
hist(obs_quantile_dm, breaks = seq(0,1,0.05), main="With obs error") #with observation error

#plot of mean pred vs. obs
plot(exp(ys),obs_pi[2,], main="Mean pred vs. obs with obs error") 

## qqplot - plot of quantiles of data in distribution including obs error
plot(seq(0,1,length.out = length(sort(obs_quantile_dm))),sort(obs_quantile_dm), main="QQplot",
xlab = "Theoretical Quantile with Obs. error",
ylab = "Empirical Quantile")
abline(0,1)

######STOPPED ADAPTING HERE

## time series 
date=as.character(dat$date)
dates<-as.Date(date)
dates1 <- dates[-c(1,21,41,61,81,101)]
par(mar = c(3,4,4,4))
plot(dates1, obs_quantile_dm,main = "dots = obs. quantiles w/ dm, triangles = gloeo counts",
     ylab = "",xlab = "",cex.main = 0.9)
mtext("obs. quantile", side=2, line=2.2)
par(new = TRUE)
plot(dates1, exp(perc_ys), axes = FALSE, bty = "n", xlab = "", ylab = "",pch = 17, col = "red", cex = 0.8)
axis(side=4, at = pretty(perc_ys))
mtext("gloeo density", side=4, line=2.2)

hist(obs_pi[2,],breaks = 20, xlim = c(0,100), main = "Mean predicted value w/ dm")

dev.off()

