#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel, Eric Lin 
#
#       File: runTestData.R
#
#       Function: Code to run as alternate to data_col.R for testing functions
#                 will read historical data into dataframes and pipe as if recieving   
#                 from socket one point at a time so can simulate real time data. 
#
#
#------------------------------------------------------------------------------------------




setwd("C:/Users/s2783343/Documents/ARC/Current System/sentineltest (r program)")

### create dataset like in data_col.R
# // create the sensor names in the order they will appearing the data structure. - if reading from directory
## // they will go in alphabetical order
####################### FUTURE CONFIG FILE ##################

##// all the possible sensors - as given from the Delphi Server - DO NOT EDIT THESE !!!!!!!!!!!!!!!!
sensor.names.all <- c("TempA", "TempC", "TempC2",
                      "pH", "pH2", "Cond", "Cond2","TurbA", 
                      "TurbA2","TurbS1", "TurbS2")  

alert.func.name <- c("TempA1", "TempC1", "TempC2",
                     "pH1", "pH2", "EC1", "EC2", "TurbA", 
                     "TurbA2","TurbS1", "TurbS2")

file.names   <- c("TempA1", "TempC1", "TempC2",
                  "pH1", "pH2", "Cond1", "Cond2", "TurbA", 
                  "TurbA2","TurbS1", "TurbS2")


getdataTEST  <- function(dataset, points = 1){
  
  dflist <- llply(dataset, head, points)
  
  return(dflist)
  
  
}
##-------------------------------------------------------------------------
##  Librarys needed
##-------------------------------------------------------------------------
library(utils)
library(gtools)
library(caTools)
#library(grDevices)
#library(rJava)
#library(rJython)
#library(RMySQL)

source("functions.R")
source("functionsGeneric.R")



library(plyr)

### Load the test Data
setDir <- "C:/Users/s2783343/Documents/ARC/Current System/sentineltest (r program)/test data"
setwd(setDir)
dataframes <- list.files(path=setDir, pattern = "dataset_")
##names <- unlist(strsplit(dataframes, "_"))
### <<< ???  can't figure out how to get names for colums from dataframe - so made headers as easy fix
dataset3 <- setNames(dataset3, dataframes)
dataset3 <- llply(dataframes, read.table,  header = T, sep = "")


dataset3 <- sapply(c("Cond1","pH1","TempC1","TurbS1"),function(x) NULL)



## print dataset to see read correctly
llply(dataset3, head, n = 1)

### how many sensors are we working with
n <- length(dataset3)

## get the names of the sensors 
sensor.config <- unlist(lapply(dataset3, colnames) [[1]][3])

#match the names
name.i <- match(sensor.config,file.names)  ### position to get the sensor correct names from both 

#c("Cond1","pH1", "TempC1","TurbS1")

##-------------------------------------------------------------------------
##  load data and workspace
##-------------------------------------------------------------------------

### example using hash - meant to be faster ?
#library(hash)
#h <- hash()

#.set( h, keys=letters, values=1:26 )
#.set( h, a="foo", b="bar", c="baz" )
#.set( h, c( aa="foo", ab="bar", ac="baz" ) )
#clear(h)
#.set( h, letters, values )


kPeriod<-1440   ####################### FUTURE CONFIG FILE ##################


alarms <- vector("list", n)
UPDATE <- vector("logical", n)
dataset <- vector( "list", n )


### working with lists of data frames  - good links
## // https://freshbiostats.wordpress.com/2012/12/21/handling-many-data-frames-in-r/
## http://stackoverflow.com/questions/9002227/how-to-get-the-name-of-a-data-frame-within-a-list



#names <- llply(dataframes, split, "_")

len <- 1
n <- length(dataset.mix)
plotwin <- 1000

while(len > 0){
    dataset.mix <- llply(dataset3, head, n =1)
    
    for( i in 1 ){
  
        dataset[[i]] <- rbind(dataset[[i]], dataset.mix[[i]])
        
        len2 <- length(dataset[[i]]$MINUTES)
        end <- length(dataset3[[i]]$MINUTES)
        end
        start <- 1
        dataset3[[i]] <- dataset3[[i]][-1,]
        
        if(len2<1000) plotwin <- 0
    
        if(len2>241){
          dist <- paste("alerts." , alert.func.name[[name.i[i]]], sep="")
          do.call( dist, list(dataset[[i]] , len2 ))     
        
       # plot(dataset[[i]]$MINUTES,dataset[[i]][[3]],xlab=sensor.config[i])
        Sys.sleep(.05)
       plotting(dataset[[i]], 3, alarms[[i]],0 , len2, setdims=FALSE, label=sensor.config[i]) 
        }
       
    # llply(dataset, plotting)  #, alarms, 0 , len2, setdims=FALSE, label=sensor.config) 

  
  }
  
  
  len <- length(dataset3[[2]]$MINUTES)
  print(len)
 

}




