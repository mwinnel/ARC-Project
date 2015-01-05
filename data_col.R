#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel, Eric Lin 
#
#       File: data_col.R
#
#       Function: global variables and environment setup. 
#
#       IMPORTANT Notes:  Please edit with caution.
#
#
#------------------------------------------------------------------------------------------


##-----------------------------------------------------------------------------------------
##  Librarys needed
##-----------------------------------------------------------------------------------------

library(utils)
library(gtools)
library(caTools)
library(grDevices)
#library(rJava)
#library(rJython)
#library(RMySQL)


##-----------------------------------------------------------------------------------------
##  Function Files
##-----------------------------------------------------------------------------------------

source("functions.R")
source("functionsGeneric.R")


##-----------------------------------------------------------------------------------------
##  Some Globals - PLEASE DO NOT CHANGE !!!!!!!!!!!!!!!!!!!!!!!!!
##-----------------------------------------------------------------------------------------

sensor.names.all <- c("TempA", "TempC", "TempC2",
                      "pH", "pH2", "Cond", "Cond2","TurbA", 
                      "TurbA2","TurbS", "TurbS2")  

alert.func.name <- c("TempA1", "TempC1", "TempC2",
                     "pH1", "pH2", "EC1", "EC2", "TurbA", 
                     "TurbA2","TurbS1", "TurbS2")

file.names   <- c("TempA1", "TempC1", "TempC2",
                  "pH1", "pH2", "Cond1", "Cond2", "TurbA", 
                  "TurbA2","TurbS1", "TurbS2")


##-----------------------------------------------------------------------------------------
##  CONGIF GLOBALS -- Edit sensor.config to be the sensors to include
##-----------------------------------------------------------------------------------------

sensor.config <- c("TempC", "pH", "Cond", "TurbS",
                   "TempC2", "pH2", "Cond2", "TurbS2")   # order matters!!! effects how plots output

kPeriod<-1440  

name.i <- match(sensor.config,sensor.names.all)  ### index to get the sensor name variations. 
n <- length(sensor.config)  # how many sensors in this unit


dataset <- sapply(sensor.config,function(x) NULL)
alarms <- sapply(sensor.config,function(x) NULL)
UPDATE <- vector("logical", n)


### *** NOTE: Can use llply and not have to do for loops anymore - or sapply
for( i in 1:n ){
  
  dataset[[i]] <- data.frame(matrix(0, nrow = 0, ncol = 4))
  colnames(dataset[[i]]) <- c("Date", "Time", sensor.names.all[name.i[i]], "MINUTES")
  alarms[[i]] <- as.data.frame(matrix(0, nrow = 0, ncol = 4))
  colnames(alarms[[i]]) <- c("MINUTES", sensor.names.all[name.i[i]], "DIRECTION", "lenTIME")
  
}


##-----------------------------------------------------------------------------------------
##  Creat socket for recieve live data - and main plotting window
##-----------------------------------------------------------------------------------------


sensor.socket <- make.socket(host = "localhost", port = 8888)
read.socket(sensor.socket)


windows(width = 60, height = 40)   

if( (n %% 2)==0 ) {
  par(mfcol = c(n/2,2))  
}else{
  par(mfcol = c(n,1))
}




##-----------------------------------------------------------------------------------------
## counters  -  *this is messy - replace with hash table or list
##-----------------------------------------------------------------------------------------
countAB.pH1 <- 0
countBW.pH1 <- 0

countAB.pH2 <- 0
countBW.pH2 <- 0

countAB.EC1 <- 0
countBW.EC1 <- 0

countAB.EC2 <- 0
countBW.EC2 <- 0

countAB.TurbS1 <- 0
countBW.TurbS1 <- 0

countAB.TurbS2 <- 0
countBW.TurbS2 <- 0

countAB.TempC1 <- 0
countBW.TempC1 <- 0

countAB.TempC2 <- 0
countBW.TempC2 <- 0

month.in.database <- 0



##-----------------------------------------------------------------------------------------
## ALARMS / bounds / probabilities  - *messy replace with list or hash table
##-----------------------------------------------------------------------------------------

system.codes2 <- as.data.frame(matrix(0, nrow = 1, ncol = 3))
colnames(system.codes2) <- c("MINUTES", "Codes", "Action")


probs.pH = c(.05,.95,.05,.95)
probs.EC = c(.05,.95,.10,.90)
probs.TurbS = c(.05,.95,.05,.95)
probs.TempC =  c(.05,.95,.05,.95)
probs.DisOxy = c(.05,.95,.05,.95)
probs.Redox =  c(.05,.95,.05,.95)


bounds.pH = c(6.3,8.1)
bounds.EC = c(940,3000)
bounds.TurbA = c(50,100)
bounds.TurbS = c(30, 600)
bounds.TempA = c(20,24.6)
bounds.TempC = c(19,22)
bounds.DisOxy = c(0.0,2)
bounds.Redox = as.numeric(c("-0.3800", "-0.28995" ) )


