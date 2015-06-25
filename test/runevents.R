# setwd("C:/SentinelTest")
setDir <- "C:/Users/s2783343/Documents/ARC/ARC-Project/ARC-Project Development/"
src <- "src"

setwd(paste(setDir,"src",sep="/"))


## get historic event from database
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='WQIAS', user='postgres', password='griffith24')
event <<- dbGetQuery(con, "SELECT * FROM events.syd2event_6")

source(paste(setDir,"src","/data_col.R",sep=""))
source(paste(setDir,"test","/loop2.R",sep=""))
