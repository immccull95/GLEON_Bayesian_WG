############################ Create correlation matrices ####################################
# Date: 09-May-18
# updated:
# Modified by: LSB
# Function source: http://myowelt.blogspot.com.br/2008/04/beautiful-correlation-tables-in-r.html
################################################################################################

### PS: needd to be adapted to Sunapee data ###

#Read data
data<-read.delim("dh_day.txt")#,row.names="dateTime")


# Figures -----------------------------------------------------------------
for (i in 2:21){
  h<-names(data[i])
  file<-paste0("fig_/",h,".jpg")
  print(file)
  jpeg(file,quality=100)
  plot(data[,i],data$GPP,ylab='GPP',xlab=h)
  dev.off()
}


# Correlation matrices function ------------------------------------------------------------------
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x,type="spearman")$r #can change for "pearson" in case one wants to
  p <- rcorr(x,type="spearman")$P #can change for "pearson" in case one wants to
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}


# Calculate the correlation ---------------------------------------------------
correl<-corstarsl(data[,2:25])


# Save the results ---------------------------------------------------------
write.csv(correl,file="spearman_dh_day.csv")
