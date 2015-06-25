###### NEED TO BE SET #################
###good bounds values for Barrier 1.
bounds.pH = c(6.3,8.1)
bounds.EC = c(940,3000)
bounds.TurbA = c(50,100)
bounds.TurbS = c(30, 600)
bounds.TempA = c(20,24.6)
bounds.TempC = c(19,22)
bounds.DisOxy = c(0.0,2)
bounds.Redox = as.numeric(c("-0.3800", "-0.28995" ) )

ph.second.LOW <- 6.1
ph.second.HIGH <- 8.9 

##-------------------------------------------------------------------------
##  CONSTANTS   & flags
##-------------------------------------------------------------------------
count <- 0
period = 720

variables <- c("pH", "Conductivity", "Turbidity", "Tempurature", "DisOxy", "Redox")
TempC.Redox.DO <- toDecimal( c(0,0,0,1,1,1) )
Redox.DO <-  toDecimal( c(0,0,0,0,1,1) )
set.d <- FALSE


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
#Melissa.M <- "0433735591"
#Roger.M <- "0418316114"
#Eddie.M <- ""
#Hujuin.M <- "0466745557"



#probs.pH = c(.05,.95,.05,.95)
#probs.EC = c(.05,.95,.10,.90)
#probs.TurbS = c(.05,.95,.05,.95)
#probs.TempC =  c(.05,.95,.05,.95)
#probs.DisOxy = c(.05,.95,.05,.95)
#probs.Redox =  c(.05,.95,.05,.95)

