#-----------------------------------------------------------------------------------------
#  CONGIF GLOBALS -- Edit sensor.config to be the sensors to include
#-----------------------------------------------------------------------------------------

#WA2 settings
bounds.pH2 = c(6.3,8)
bounds.EC2 = c(900,2000)
bounds.TurbS2 = c(50, 250)
bounds.TempC2 = c(20,22.8)
ph.second.LOW <- 6.1
ph.second.HIGH <- 8.9 



sensor.config <- c("TempA", "TempC", "pH", "Cond", "TurbS","TurbA",
                   "TempC2", "pH2", "Cond2", "TurbS2","TurbA2")   # order matters!!! effects how plots output - data order

### ADD IN SENSOR CONFIG FOR PLOTS _ SEPERATE FROM ALL DATA VARIABLES
sensor.plot.process <-  c("TempC", "pH", "Cond", "TurbS")  # order matters!!! effects how plots output


siteName <- "Beenyup WWTP"
kPeriod <- 1440  
RealtimeRange <- 241
RealtimeInterval <- 60
#ftpAdd <- "ftp://192.168.0.100"
Address <- "203.101.238.182"
nodeID <- "wa2"
#ftpAddress <- paste(ftpAdd,"/",nodeID,"/",sep="")
#commd <- paste("c:\\ARC\\pscp.exe -pw griffith c:\\grldr ", unit,"@",address,":test",sep="")


LIVESTREAM <- TRUE



##-------------------------------------------------------------------------
##  CONSTANTS   & flags
##-------------------------------------------------------------------------
count <- 0
period = 720

variables <- c("pH", "Conductivity", "Turbidity", "Temperature")
#TempC.Redox.DO <- toDecimal( c(0,0,0,1,1,1) )
#Redox.DO <-  toDecimal( c(0,0,0,0,1,1) )
set.d <- FALSE

current.code <- 0
###vars for multi
reporting.length.wait <- 2
buffer.period <- 15
statechange.start.minutes <- 0

emailSENT <- 0
email.int.time <- 30

#emailTO <- ''
#emailFROM <- ''

SentinelNO <- "4"
BarrierNO <- "1"

action<-"NONE"