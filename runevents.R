# setwd("C:/SentinelTest")
setDir <- "C:/Users/s2783343/Documents/ARC/ARC-Project/ARC-Project Development"

## run historic data - event
setwd(setDir)

## get historic event from database
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='WQIAS', user='postgres', password='griffith24')
event <<- dbGetQuery(con, "SELECT * FROM events.syd2event_14")


source("data_col.R")
source("loop2.R")
