#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel
#
#       File: runTestData.R
#
#       Function: Code to run as alternate to data_col.R for testing functions
#                 will read historical data into dataframes and pipe as if recieving   
#                 from socket one point at a time so can simulate real time data.
#                 using list structure
#
#       To Run:
#             setDir - need directory with data files in format "dataset_"
#             startIndex <- 1000            # start row number   
#             stopIndex <- 7000             # end row number
#             maxBufferSize <- 80           # windows size - default 2880 / 2 days data
#
#------------------------------------------------------------------------------------------


# rm(list=ls())                        # clean out the workspace
setDir <- "C:/Users/s2783343/Documents/ARC/Current System/sentineltest (r program)/test data"
setwd(setDir)


# source("functions.R")
# source("functionsGeneric.R")

getNext <- function(dataset, start.pos = 1, end.pos = 2) {  
      
  dflist <- lapply(dataset, function(x)x[start.pos:end.pos,])

  return(dflist)
  
}


####################### FUTURE CONFIG FILE ##################

# all the possible sensors - as given from the Delphi Server - DO NOT EDIT THESE !!!!!!!!!!!!!!!!
sensor.names.all <- c("TempA", "TempC", "TempC2", "pH", "pH2",
                      "Cond", "Cond2","TurbA", "TurbA2","TurbS1", "TurbS2")  

alert.func.name <- c("TempA1", "TempC1", "TempC2", "pH1", "pH2", 
                     "EC1", "EC2", "TurbA", "TurbA2","TurbS1", "TurbS2")

file.names <- c("TempA1", "TempC1", "TempC2", "pH1", "pH2",
                  "Cond1", "Cond2", "TurbA", "TurbA2","TurbS1", "TurbS2")


#-------------------------------------------------------------------------
#  Librarys + source files needed
#-------------------------------------------------------------------------
library(utils)
library(gtools)
library(caTools)
library(plyr)

# change to connect to database in future
dataframes <- list.files(path = setDir, pattern = "dataset_") 
datasetRealTime <- llply(dataframes, read.table,  header = T, stringsAsFactors = F, sep = "")
datasetRealTime <- setNames(datasetRealTime, dataframes)


llply(datasetRealTime, head, n = 2)              ## print first point in each of data set list        

# how many sensors are we working with
n <- length(datasetRealTime)

# get the names of the sensors 
sensor.config <- 1
for (i in 1:n) {
  sensor.config[i] <- unlist(lapply(datasetRealTime, colnames) [[i]][3])
}


# match the names to their index position
name.i <- match(sensor.config, file.names)  


#-------------------------------------------------------------------------
#  load data and workspace
#-------------------------------------------------------------------------

startIndex <- 1               # set start row index   
stopIndex <- 7000             # set end row index
maxBufferSize <- 2880           # we keep 2880 points maximum in memory
startIndexAdj <- startIndex

for (i in startIndex:stopIndex ) {  
  
 
    print(i)
    dataset.mix <- getNext(datasetRealTime, startIndexAdj, i)
    
     if (i > maxBufferSize+startIndex) {
       startIndexAdj <- startIndexAdj + 1  # shift the window by 1
     }
   
    
    #------------------------------------------------------------------------------------------
    
    # PASTE FUNCTIONS HERE TO TEST - dataset.mix is a list containing your data
    # if not using lists extract to data files
    # example if only want ph 
    
    llply(datasetRealTime, head, n = 1)              # view first point in each of data set list        
    data.pH1 <- dataset.mix$dataset_pH1.dat
    data.TempC1 <- dataset.mix$$dataset_TempC1.dat
    
    
    
    
    
    plot(data.pH1$MINUTES, data.pH1$pH1, type = "l" )
    Sys.sleep(.1)

    
    #------------------------------------------------------------------------------------------
   
    
}






