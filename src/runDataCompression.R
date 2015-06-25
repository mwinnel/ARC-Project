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

#rm(list=ls(all=TRUE))
library(RCurl)
library(R.utils)
library(TTR)


source("douglasFunction.R")
#source("functions.R")
#-----------------------------------------------------------------------------------------
#  Some Globals - PLEASE DO NOT CHANGE !!!!!!!!!!!!!!!!!!!!!!!!!
#-----------------------------------------------------------------------------------------
compressiontype <- "all"

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

sensor.config <- c("TempA", "TempC", "TempC2",
                   "pH", "pH2", "Cond", "Cond2","TurbA", 
                   "TurbA2","TurbS", "TurbS2")    # order matters!!! effects how plots output

ftpAdd <- "ftp://192.168.30.11"
nodeID <- 4
ftpAddress <- paste(ftpAdd,"/",nodeID,"/",sep="")




#epsilon is used to set the compression strength. It is based on the data range.
epsilon <- 0.1

start.minute <- read.table("minute.txt", header=FALSE)
#kPeriod <- 1440  
dataset <<- sapply(sensor.config,function(x) NULL)
name.i <- match(sensor.config,sensor.names.all)  ### index to get the sensor name variations. 
n <- length(sensor.config)  # how many sensors in this unit


######### READ IN FILES - LAST num.points

for(j in 1:n) {  
  file <- paste("dataset_" , file.names[[name.i[j]]], ".dat", sep="")
  df.names <- c("Date", "Time", sensor.names.all[name.i[j]], "MINUTES")
  
  dataset.previous <- read.table(file, col.names=df.names,sep="")
  set.df <- dataset.previous[dataset.previous$MINUTES >= start.minute[1,],]
  
  #remove NA rows
  row.has.na <- apply(set.df, 1, function(x){any(is.na(x))})
  sum(row.has.na)
  set.df <- set.df[!row.has.na,]
  
  
  if(nrow(set.df)>0)
  {
    
    if(compressiontype == "douglas"){
      compressed <- run.douglas(set.df)
    }
    
    else{
      
      ## loop and save average 1 var per 10 mins - or use kalman smoother
      
      #send whole data set
      compressed <- set.df
      
    }

    csvname2 <-  paste("douglas_",file.names[name.i[j]],".csv", sep="")
    write.csv(compressed, file = csvname2, row.names = FALSE)
    last.minute <- compressed[dim(compressed)[1],4]
    write.table(last.minute, file = "minute.txt", row.names = FALSE,col.names=FALSE)
    
  }else{
    
    print("zero to update")  
    
  }
}



## ATTEMPT TO SEND TO SERVER
## include douglas algorithms - or open R Work Space with them included.
files <- c("dataset_Cond1.csv","dataset_Cond2.csv","dataset_pH1.csv","dataset_pH2.csv","dataset_TurbS1.csv","dataset_TurbS2.csv","dataset_TurbA1.csv","dataset_TurbA2.csv","dataset_TempC1.csv","dataset_TempC2.csv","dataset_TempA1.csv")
#files <- paste("dataset_",file.names,".csv",sep="")
D_files <- paste("douglas_",file.names,".csv", sep="")
D_tgzName <- "Douglas_data.tgz"

ToTgz(D_tgzName,D_files)

tryCatch({ ftpUpload(D_tgzName, paste(ftpAddress,D_tgzName,sep=""))}, condition=function(ex) {
  a <- print(ex)
  write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE);
  #ftp failed so set to start.minute to do again next time
  write.table(start.minute, file = "minute.txt", row.names = FALSE,col.names=FALSE)
   # after send delete files?
  
  
  })











