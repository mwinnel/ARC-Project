#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel
#
#       File: runDataCompression.R
#
#       Function: Data Compression algorithm - follows the Douglas Algorithm
#
#
#------------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))
library(data.table)

source("douglasFunction.R")

## include douglas algorithms - or open R Work Space with them included.



#epsilon is used to set the compression strength. It is based on the data range.
#lastoneupdate<<-FALSE


start.minute <- read.table("minute.dat", header=FALSE)
start.minute<-0


## THIS NOT WORKING
if(start.minute == 0 & dim(dat)[1] > (2880*2) ) { 
  data.set <- dat[(dim(dat)[1]-(2880*2):dim(dat)[1]),] 
} else {
  data.set <- dat[dat$MINUTES > start.minute[1,],] 
} 

if(dim(data.set)[1] <= 3)
{
  ##exit - nothing to update
  print("nothing to update")
  
}







## LOOP THROUGH AND READ EACH FILE AND COMPRESS


#-----------------------------------------------------------------------------------------
#  Some Globals - PLEASE DO NOT CHANGE !!!!!!!!!!!!!!!!!!!!!!!!!
#-----------------------------------------------------------------------------------------

sensor.names.all <- c("TempA", "TempC", "TempC2",
                      "pH", "pH2", "Cond", "Cond2","TurbA", 
                      "TurbA2","TurbS", "TurbS2")  

alert.func.name <- c("TempA1", "TempC1", "TempC2",
                     "pH1", "pH2", "EC1", "EC2", "TurbA", 
                     "TurbA2","TurbS1", "TurbS2")

file.names <- c("TempA1", "TempC1", "TempC2",
                  "pH1", "pH2", "Cond1", "Cond2", "TurbA1", 
                  "TurbA2","TurbS1", "TurbS2")


#-----------------------------------------------------------------------------------------
#  CONGIF GLOBALS -- Edit sensor.config to be the sensors to include
#-----------------------------------------------------------------------------------------

sensor.config <- c("TempA", "TempC", "pH", "Cond", "TurbS","TurbA",
                    "TempC2", "pH2", "Cond2", "TurbS2","TurbA2")   # order matters!!! effects how plots output

#kPeriod <- 1440  
dataset <<- sapply(sensor.config,function(x) NULL)
name.i <- match(sensor.config,sensor.names.all)  ### index to get the sensor name variations. 
n <- length(sensor.config)  # how many sensors in this unit


######### READ IN FILES - LAST num.points

num.points <- 1440     ##  HOW MANY POINTS TO READ IN ??




for(j in 1:n) {
  
  
  file <- paste("dataset_" , file.names[[name.i[j]]], ".dat", sep="")
  df.names <- c("Date", "Time", sensor.names.all[name.i[j]], "MINUTES")
  
 # print("filename")
 # print(file)
 # print("sensor name")
 # print(df.names)
  dataset.previous <- read.table(file, col.names=df.names )
  lenp <- dim(dataset.previous)[1]
  
  ## (to do) CHECK IF FILE EXISTS
  newdataset <- scan(file, '', skip = lenp-num.points, nlines = num.points, sep = '\n')
  
  set.df <- matrix(NA, nrow=0, ncol=4)
  for(i in 1:num.points){
    
    a <- unlist(strsplit(newdataset[i], " "))
    newdata <- cbind(a[1], a[2], as.double(a[3]),as.numeric(a[4]))
    set.df <- rbind(set.df, newdata)
  }
  
  ## dataset is our main data frame - do not delete or remove
  dataset[[j]] <- cbind(as.data.frame(set.df[, 1:2]), as.double(set.df[, 3]),as.double(set.df[, 4]))
  colnames(dataset[[j]]) <- df.names
  #print(head(dataset[[j]]))
  
  
  
  ################  RUN DOUGLAS
  compressed <- run.douglas(dataset[[j]])
  
  
  ############# SAVE RESULTS
  
    ## APPEND TRUE OR FALSE ???
    
    write.table(compressed, paste("dataset_douglas_" , file.names[[name.i[j]]], ".dat", sep=""), row.names = FALSE, append = FALSE, col.names = FALSE)
   # write.table(last.minute, "minute.dat", row.names = FALSE, append = FALSE, col.names = FALSE)
 
  
}






  

#plot(dataset.TempC1[,4],dataset.TempC1[,3],typ="l")
#points(result, col="red")

last.minute <- compressed.data[dim(compressed.data)[1],4]









 lapply(dataset, head, n=1)
 
   # do some cleanup // note: does moving to function auto cleans at return...(check this)
  rm(dataset.previous, newdataset, newdata, set.df, a, lenp, file, df.names)  

 
for(j in 1:n) {
  ## plot data
  
  plotting(dataset.previous[[j]], 3, alarms[[j]], kPeriod,241, 241, label=sensor.names.all[name.i[j]])
  
  
}



win.graph()
plot(data,typ="l", axes = FALSE) 
lines(result, col="blue")
points(result, col="blue")
axis(2)
box()
LabelTimeAxis()




