plot(event$MINUTES,event$pH)
n <- rollapply(event$pH, width = 2, FUN = diff, fill=NA)


for(i in 3:7){
  
  event[[3]] <- as.numeric(as.character(event[[3]]))
  
  
}

http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#differencing-a-time-series


kings <- scan("C:/Users/s2783343/Downloads/kings.dat",skip=3)

births <- scan("C:/Users/s2783343/Downloads/nybirths.dat")

birthstimeseries <- ts(births, frequency=1440, )

event <<- dbGetQuery(con, "SELECT * FROM sydneywater.sydney2_ph1")
event <<- dbGetQuery(con, "SELECT * FROM events.event8")
ph <- ts(event$pH[120000:140000], frequency=1440)
phd <- decompose(ph)
plot(phd)
phSMA8 <- SMA(ph,n=8)


plot(n)
library(zoo)
