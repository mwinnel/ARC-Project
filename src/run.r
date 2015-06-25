# setwd("C:/SentinelTest")
setDir <- "C:/Users/s2783343/Documents/ARC/ARC-Project/ARC-Project Development/"
src <- "src"
setwd(paste(setDir,"src",sep="/"))

source(paste(setDir,"src","/data_col.R",sep=""))
setwd(paste(setDir,"data/",sep=""))
source(paste(setDir,"src","/start_spot.R",sep=""))
source(paste(setDir,"src","/loop.R",sep=""))
