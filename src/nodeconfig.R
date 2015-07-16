#-----------------------------------------------------------------------------------------
#  CONGIF GLOBALS -- Edit sensor.config to be the sensors to include
#-----------------------------------------------------------------------------------------

sensor.config <- c("TempA", "TempC", "pH", "Cond", "TurbS","TurbA",
                   "TempC2", "pH2", "Cond2", "TurbS2","TurbA2")   # order matters!!! effects how plots output - data order

### ADD IN SENSOR CONFIG FOR PLOTS _ SEPERATE FROM ALL DATA VARIABLES
sensor.plot.process <-  c("TempC", "pH", "Cond", "TurbS")  # order matters!!! effects how plots output


siteName <- "melissa"
kPeriod <- 1440  
RealtimeRange <- 241
RealtimeInterval <- 60
ftpAdd <- "ftp://192.168.30.10"
nodeID <- 5
ftpAddress <- paste(ftpAdd,"/",nodeID,"/",sep="")

