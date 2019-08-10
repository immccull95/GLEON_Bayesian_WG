#developing prior for observational Gloeo error
library(tidyverse)
library(readxl)

#using data from Kathy 01JUL19

gloeo <- read_xlsx("./Datasets/Sunapee/SummarizedData/CompareMidgeDailytoWeeklyGloeoSamples_17April2019.xlsx")

hist(gloeo$OurWeekly_GloeoDensity, breaks = 30)

diff = abs(gloeo$OurWeekly_GloeoDensity - gloeo$MidgeDaily_GloeoDensity)

hist(diff, breaks = 30)

1/(mean(diff, na.rm = TRUE)^2)
#0.519 in log space; using this for prior?
#155.0856 in not-log space
#1.79 in densities

sd(diff, na.rm = TRUE)
#1.006; using this for prior?
#908.4293 in not-log space
#6.23 in densities


1/(mean(diff, na.rm = TRUE)^2)

#instead model this hierarchically and let information from the other
#dataset inform this one, because can't technically develop a prior
#using just the two data points - look at mouse example in hierarchical
#modeling slides


##NO!!!!
#instead calibrate the model on newbury or some other site and then 
#use that as the prior for midge :-)

#using fichter because that converged well
#for tau_obs
#mean = 0.74827
#SD = 0.1392044
#needs to be converted to shape and rate for gamma prior