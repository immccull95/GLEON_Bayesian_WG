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
source('RCode/helper_functions/seasonal_plug_n_play_2015_validation.R')


#1) Model options => pick date range, site, time step, and type of model -----------------------------------------------------

model_name = 'Seasonal_MIn_MinSchmidt_Diff' # options are RandomWalk, RandomWalkZip, Logistic, Exponential, DayLength, DayLength_Quad, RandomYear, TempExp, Temp_Quad,  ChangepointTempExp
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
Wnd <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_NLDASminwind_03MAR20.csv"))
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
y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003) 
y <- y[7,]
year_no = 1
season_weeks = c(1:20)


dat <- read_csv("./Datasets/Sunapee/SummarizedData/seasonal_data_temp_forecast.csv") %>%
  filter(site == "midge", year == 2015)

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

Temps <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_watertemp_min_forecast_05OCT19.csv"))
Temps <- scale(Temps, center = TRUE, scale = TRUE)
Temps <- c(Temps[7,])

#max Schmidt
Schmidts <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_max_Schmidt_forecast_04MAR20.csv"))
Schmidts <- scale(Schmidts, center = TRUE, scale = TRUE)
Schmidts <- c(Schmidts[7,])

#min Schmidt
Schmidts <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Buoy_year_by_week_min_Schmidt_forecast_04MAR20.csv"))
Schmidts <- scale(Schmidts, center = TRUE, scale = TRUE)
Schmidts <- c(Schmidts[7,])

Wnds <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_NLDASminwind_forecast_03MAR20.csv"))
Wnds <- scale(Wnds, center = TRUE, scale = TRUE)
Wnds <- c(Wnds[7,])

GDDs <- as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/GDD_year_by_week_forecast_03MAR20.csv"))
GDDs <- scale(GDDs, center = TRUE, scale = TRUE)
GDDs <- c(GDDs[7,])

SWs <- as.matrix(read_csv("./Datasets/Sunapee/Bayes_Covariates_Data/Midge_year_by_week_SW_forecast_03MAR20.csv"))
SWs <- scale(SWs, center = TRUE, scale = TRUE)
SWs <- c(SWs[7,])

y <- log(as.matrix(read_csv("./Datasets/Sunapee/SummarizedData/Midge_year_by_week_totalperL_forecast_05OCT19.csv"))+0.003)
ys <- y[7,]

#get one-step-ahead predictions
preds_plug_ins <- preds_plug_ins(model_name) 

pi <- exp(apply(preds_plug_ins$pred.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))
obs_pi <- exp(apply(preds_plug_ins$pred_obs.model,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))

pi <- apply(pred,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)
obs_pi <- apply(pred_obs,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE)

exp_pi <- exp(apply(pred,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))
exp_obs_pi <- exp(apply(pred_obs,2,quantile,c(0.025,0.5,0.975), na.rm=TRUE))



#7) CI, PI, Obs PI Plots


#CI, PI, Obs PI
png(file=file.path(my_directory,paste(site,paste0(model_name,'_PI_2015.png'), sep = '_')), res=300, width=20, height=15, units='cm')
par(mfrow = c(2,1), oma = c(1,1,5,1), mar = c(4,4,2,2)+0.1)

#2015 log
plot(times[1:20],obs_pi[2,1:20],type='n', ylab="log Gloeo density (total per L)", ylim = c(min(obs_pi[1,1:20], na.rm = TRUE),max(obs_pi[3,1:20], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[1:20],obs_pi[1,1:20],obs_pi[3,1:20],col="gray")
ciEnvelope(times[1:20],pi[1,1:20],pi[3,1:20],col="Green")
points(times[1:20],exp(ys[1:20]),pch="+",cex=0.8)
legend("topleft",legend = "2015", bty = "n")
points(times[1:20],obs_pi[2,1:20],pch = 5, cex = 0.8)

#2015 exp
plot(times[1:20],exp_obs_pi[2,1:20],type='n', ylab="Gloeo density (total per L)", ylim = c(min(exp_obs_pi[1,1:20], na.rm = TRUE),max(exp_obs_pi[3,1:20], na.rm = TRUE)),
     main="",xlab = "")
ciEnvelope(times[1:20],exp_obs_pi[1,1:20],exp_obs_pi[3,1:20],col="gray")
ciEnvelope(times[1:20],exp_pi[1,1:20],exp_pi[3,1:20],col="Green")
points(times[1:20],exp(ys[1:20]),pch="+",cex=0.8)
legend("topleft",legend = "2015", bty = "n")
points(times[1:20],exp_obs_pi[2,1:20],pch = 5, cex = 0.8)

dev.off()



#8) Further Diagnostic Checks and Visualization 

#y vs. preds
ys = ys
obs_diff= vector(mode="numeric", length=20)
obs_quantile = vector(mode="numeric", length=20)
obs_quantile_dm = vector(mode="numeric",length=20)
pred_mean = vector(mode="numeric",length=20)
mypreds <- pred
mypreds_obs <- pred_obs
perc_ys <- ys
obs_pi1 <- obs_pi[1,]
obs_pi3 <- obs_pi[3,]





for(i in 2:ncol(mypreds)){
  obs_diff[1]<-NA
  obs_quantile[1]<-NA
  obs_quantile_dm[1]<- NA
  pred_mean[1]<- NA
  
  obs_diff[i]=mean(exp(mypreds_obs[,i]))-exp(perc_ys[i]) #difference between mean of pred. values and obs for each time point
  pred_mean[i]=mean(exp(mypreds[,i])) #mean of pred. values at each time point
  percentile <- ecdf(exp(mypreds[,i])) #create function to give percentile based on distribution of pred. values at each time point
  obs_quantile[i] <- percentile(exp(perc_ys[i])) #get percentile of obs in pred distribution
  percentile1 <- ecdf(exp(mypreds_obs[,i])) #create function to give percentile of obs in distribution of pred including observation error
  obs_quantile_dm[i] <- percentile1(exp(perc_ys[i])) #get percentile of obs 
}

sink(file = file.path("Results/Jags_Models/Final_analysis",paste(site,paste0(model_name,'_obs_pred_differences_2015.txt'), sep = '_')))

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
print("Quantile of 2015 maximum density in distribution of pred including observation error")
obs_quantile_dm[17]

#percent of time we are predicting negative Gloeo
print("Percent of time we are predicting negative Gloeo")
length(subset(exp(obs_pi[2,]),exp(obs_pi[2,]) < 0))/length(obs_pi[2,])*100

#correlation coefficient of pred vs. obs
cor.coef <- cor(exp(perc_ys),exp(obs_pi[2,]), method = "pearson", use = "complete.obs")
print("Pearson's correlation coefficient of observations and 50th quantile of predicted")
cor.coef

#Mean range of 95% predictive interval
mean_range_pred <- mean(exp(obs_pi3) - exp(obs_pi1), na.rm = TRUE)
print("Mean range of 95% confidence interval including observation error")
mean_range_pred

sink()


#9) Diagnostic Plots

png(file=file.path(my_directory,paste(site,paste0(model_name,'_Diagnostics_2015.png'), sep = '_')), res=300, width=20, height=15, units='cm')
par(mfrow=c(3,2))

#hist of quantiles
hist(obs_quantile, breaks = seq(0,1,0.05),main="No obs error") #no observation error
hist(obs_quantile_dm, breaks = seq(0,1,0.05), main="With obs error") #with observation error

#plot of mean pred vs. obs
plot(exp(ys),exp(obs_pi[2,]), main="Mean pred vs. obs with obs error") 

## qqplot - plot of quantiles of data in distribution including obs error
plot(seq(0,1,length.out = length(sort(obs_quantile_dm))),sort(obs_quantile_dm), main="QQplot",
xlab = "Theoretical Quantile with Obs. error",
ylab = "Empirical Quantile")
abline(0,1)

######STOPPED ADAPTING HERE

## time series 
date=as.character(dat$date)
dates<-as.Date(date)
par(mar = c(3,4,4,4))
plot(dates, obs_quantile_dm,main = "dots = obs. quantiles w/ dm, triangles = gloeo counts",
     ylab = "",xlab = "",cex.main = 0.9)
mtext("obs. quantile", side=2, line=2.2)
par(new = TRUE)
plot(dates, exp(perc_ys), axes = FALSE, bty = "n", xlab = "", ylab = "",pch = 17, col = "red", cex = 0.8)
axis(side=4, at = pretty(perc_ys))
mtext("gloeo density", side=4, line=2.2)

hist(exp(obs_pi[2,]),breaks = 20, main = "Mean predicted value w/ dm")

dev.off()

