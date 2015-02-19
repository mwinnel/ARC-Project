#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel
#
#       File: runTestData.R
#
#       Function: Code to run as alternate to data_col.R for testing functions
#                 will read historical data into dataframes and pipe as if recieving   
#                 from socket one point at a time so can simulate real time data. 
#       To Run:
#             setDir - working directory with data  
#             pos <- 1000               # start spot in the array   
#             end.points <- 1440        # set the end row position
#
#
#
#------------------------------------------------------------------------------------------


#rm(list=ls())                        # clean out the workspace
setDir <- "C:/Users/s2783343/Documents/ARC/Current System/sentineltest (r program)/test data"
setwd(setDir)


#source("functions.R")
#source("functionsGeneric.R")

getNext  <- function(dataset, points = 1){  
  
  ## take the head points off the dataset
  dflist <- llply(dataset, head, points)
  return(dflist)
  
}




####################### FUTURE CONFIG FILE ##################

# all the possible sensors - as given from the Delphi Server - DO NOT EDIT THESE !!!!!!!!!!!!!!!!
sensor.names.all <- c("TempA", "TempC", "TempC2", "pH", "pH2",
                      "Cond", "Cond2","TurbA", "TurbA2","TurbS1", "TurbS2")  

alert.func.name <- c("TempA1", "TempC1", "TempC2", "pH1", "pH2", 
                     "EC1", "EC2", "TurbA", "TurbA2","TurbS1", "TurbS2")

file.names   <- c("TempA1", "TempC1", "TempC2", "pH1", "pH2",
                  "Cond1", "Cond2", "TurbA", "TurbA2","TurbS1", "TurbS2")


##-------------------------------------------------------------------------
##  Librarys + source files needed
##-------------------------------------------------------------------------
library(utils)
library(gtools)
library(caTools)
library(plyr)


dataframes <- list.files(path=setDir, pattern = "dataset_") 
datasetRealTime <- llply(dataframes, read.table,  header = T,stringsAsFactors=F, sep = "")
datasetRealTime <- setNames(datasetRealTime, dataframes)


llply(datasetRealTime, head, n = 1)              ## print first point in each of data set list        

# how many sensors are we working with
n <- length(datasetRealTime)

# get the names of the sensors 
sensor.config <- 1
for(i in 1:n){
  sensor.config[i] <- unlist(lapply(datasetRealTime, colnames) [[i]][3])
}


# match the names
name.i <- match(sensor.config,file.names)  ### position to get the sensor correct names from both 


#-------------------------------------------------------------------------
#  load data and workspace
#-------------------------------------------------------------------------
pos <- 1000               # start spot in the array   
end.points <- 1440        # set the end row position


for(i in pos:end.points ){  
  
    print(i)
    dataset.mix <- getNext(datasetRealTime, i)

    #------------------------------------------------------------------------------------------
    
    # PASTE FUNCTIONS HERE TO TEST - dataset.mix is a list containing your data
    # if not using lists extract them to data files
    # example if only want ph 
    
    llply(datasetRealTime, head, n = 1)              ## print first point in each of data set list        
    dataset.pH1 <- dataset.mix$dataset_pH1.dat
    
    #------------------------------------------------------------------------------------------
   
    
}






