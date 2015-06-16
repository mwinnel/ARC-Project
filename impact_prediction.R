setwd("E:/plot")

epsilon <<- 0.001

douglas <- function(PointList, epsilon)
{
  dmax <- 0
  index <- 0
  end <- dim(PointList)[1]
  
  for(i in 2:(end-1))
  {
    d <- distance(PointList[i,], PointList[1,], PointList[end,]) 
    if ( d > dmax ) {
      index <- i
      dmax <- d
    }
  }
  # If max distance is greater than epsilon, recursively simplify
  if ( dmax > epsilon ) 
  {
    # Recursive call
    result2 <<- rbind(result2, PointList[index,])
    douglas(PointList[1:index,], epsilon)
    douglas(PointList[index:end,], epsilon)
    
    # Build the result list
    #ResultList <- rbind(recResults1[1:end-1,], recResults2[1:end,])
  } 
  else 
  {
    #ResultList <- rbind(PointList[1,], PointList[end,])
    return(TRUE)
  }
  # Return the result
}


distance <- function(point1,point2,point3)
{
  A <- as.numeric(point2[,2])-as.numeric(point3[,2])
  B <- as.numeric(point3[,1])-as.numeric(point2[,1])
  C <- as.numeric(point2[,1])*as.numeric(point3[,2])-as.numeric(point3[,1])*as.numeric(point2[,2])
  h <- abs(A*as.numeric(point1[,1])+B*as.numeric(point1[,2])+C)/sqrt(A*A+B*B)
  return(h)
}




#datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )


#k <- c(6, 4, 8, 12 )
#k <- c(6, 3, 5, 7 )

event <<- dbGetQuery(con, "SELECT * FROM events.syd2event_17")
eventid <- "syd2event_17"
source("fingerprint.R")


title(sub = paste("historical event FP") ,col="red", col.sub = "red", cex.sub= 2, font.sub=3)



for (i in 1:4){
data <- cbind(event[14],event[k[i]])

#print (data[,2])

#h <- distance(data[1,],data[4598,],data[10000,])
#print(h)
result <<- rbind(data[1,],data[dim(data)[1],])

douglas(data,epsilon)
result <<- result[order(result$MINUTES),]


## EXTRACT FINGERPRINT FOR EVENT ----------------------------------


######### duration
duration <- as.numeric(data[dim(data)[1],1])-as.numeric(data[1,1])

######### base line 
baseline <- (as.numeric(data[1,2])+as.numeric(data[dim(data)[1],2]))/2

######### Integration
integration <- 0
for(i in 1:dim(data)[1]){
  integration <- integration+as.numeric(data[i,2])-baseline
}

######### peak calculation
if(integration>0){
  peak <- as.numeric(max(result[,2]))
}else{
  peak <- as.numeric(min(result[,2]))
}

peak <- subset(result, result[,2] == peak)

######### speed calculation
gospeed <- (as.numeric(peak[,2])-as.numeric(result[1,2]))/(as.numeric(peak[,1])-as.numeric(result[1,1]))
returnspeed <- (as.numeric(result[dim(result)[1],2])-as.numeric(peak[,2]))/(as.numeric(result[dim(result)[1],1])-as.numeric(peak[,1]))

## plot event
plot(result,axes=F,xlab="")
lines(result,type="l",col="red")
abline(h = baseline, col = "black", lty=2)

lines(c(as.numeric(result[1,1]), as.numeric(peak[1,1])), c(as.numeric(result[1,2]), as.numeric(peak[1,2])), col="blue", typ="l")
lines(c(as.numeric(peak[1,1]),as.numeric(result[dim(result)[1],1])), c( as.numeric(peak[1,2]),as.numeric(result[dim(result)[1],2])), col="blue", typ="l")

#mtext(paste("peak=",peak$pH,"duration=",duration,"area=",integration, sep=" "),line=2)
#mtext(paste("slope=",round(gospeed[1],4),"slope 2=",round(returnspeed[1],4), sep=" "),line=1)

box()
axis(2)

}



title(sub = paste("historical event FP") ,col="red", col.sub = "red")
#LabelTimeAxis()


sensor <- colnames(result)[2] 
eventID <- "syd2event_12"
df[2,] <- c(eventID, sensor,peak[1,1],peak[1,2],gospeed[1],returnspeed[1],duration,integration,baseline,event[1,]$MINUTES, tail(event,n=1)$MINUTES)


dbWriteTable(con, c("events","impacts"), df[2,] , row.names=FALSE, append=TRUE, overwrite=FALSE)
dbWriteTable(con2, c("events","impacts"), df[2,] , row.names=FALSE, append=TRUE, overwrite=FALSE)


for(i in 3:11){
  
  impacts[,i] <- as.numeric(as.character(impacts[,i]))

  
}




df <- data.frame(matrix(vector(),11,11),stringsAsFactors=FALSE)
colnames(df) <- rowName


rowName <- c("eventID","sensor","peakMIN","peak","gspeed","rspeed","dur","integ","base","startminute","endminute")
vars <- rep(rowName,11)
vars2 <-  c(rep(sensor.config[1],6))
for(i in 2:11){
  vars2 <- c(vars2,rep(sensor.config[i],6))
}


csvname3 <- "impacts.csv"
csvname4 <- "curve.csv"
write.csv(event, file = csvname4, row.names = F)

write.table(t(impacts.dat), file = csvname3, row.names = FALSE,col.names=FALSE, append=TRUE,sep=",")


## ----------------------------
## FEATURE SET EVENT 2  ------ THIS WILL BE IN DATABASE ????  ----------------------------
## ----------------------------

data2 <- cbind(event[9],event[6])

douglas(data2,epsilon)
result2 <<- result2[order(result2$MINUTES),]

plot(data[,1],data[,2],typ="l")
lines(result,type="l",col="red")

data2 <- data
result2 <- result

######### duration
duration2 <- as.numeric(data2[dim(data2)[1],1])-as.numeric(data2[1,1])
######### base line 
baseline2 <- (as.numeric(data2[1,2])+as.numeric(data2[dim(data2)[1],2]))/2
abline(h = baseline2, col = "black", lty=2)

######### Integration
integration2 <- 0
for(i in 1:dim(data2)[1]){
  integration2 <- integration2+as.numeric(data2[i,2])-baseline2
}

######### peak calculation
if(integration2>0)
  peak2 <- as.numeric(max(result2[,2]))
if(integration2<=0)
  peak2 <- as.numeric(min(result2[,2]))

peak2 <- subset(result2, pH == peak2)
######### speed calculation
gospeed2 <- (as.numeric(peak2[,2])-as.numeric(result2[1,2]))/(as.numeric(peak2[,1])-as.numeric(result2[1,1]))
returnspeed2 <- (as.numeric(result2[dim(result2)[1],2])-as.numeric(peak2[,2]))/(as.numeric(result2[dim(result2)[1],1])-as.numeric(peak2[,1]))
lines(c(as.numeric(result2[1,1]), as.numeric(peak2[1,1])), c(as.numeric(result2[1,2]), as.numeric(peak2[1,2])), col="blue", typ="l")
lines(c(as.numeric(peak2[1,1]),as.numeric(result2[dim(result2)[1],1])), c( as.numeric(peak2[1,2]),as.numeric(result2[dim(result2)[1],2])), col="blue", typ="l")


eventID2 <- paste("event",siteName,nodeID,event.start,sep="_")
impacts.dat <- c(eventID2, sensor,peak2$MINUTES,peak2$pH,gospeed2,returnspeed2,duration2,integration2,baseline2)
impacts <- rbind(impacts,c(eventID, sensor,peak$MINUTES,peak$pH,gospeed,returnspeed,duration,integration,baseline) )

impacts_8_9 <- c(possibility,p_duration,p_baseline,p_integration,p_peak,p_gospeed,p_returnspeed)
impacts_8_6 <- c(possibility,p_duration,p_baseline,p_integration,p_peak,p_gospeed,p_returnspeed)
impacts <- as.data.frame(rbind(impacts_8_9,impacts_8_6))
colnames(impacts) <- c("possibility","p_duration","p_baseline","p_integration","p_peak","p_gospeed","p_returnspeed")

######### COMPARISON PERCENT TEST ----------------------
#set weights  --- again in databse? or have configurable defaults?
w_baseline <- 0.2
w_duration <- 0.2
w_integration <- 0.2
w_peak <- 0.2
w_gospeed <- 0.1
w_returnspeed <- 0.1

######### p_baseline
if(abs(baseline)>abs(baseline2)){
  p_baseline <- abs(baseline2)/abs(baseline)*w_baseline
}
if(abs(baseline)<=abs(baseline2)){
  p_baseline <- abs(baseline)/abs(baseline2)*w_baseline
}

######### p_duration
if(abs(duration)>abs(duration2)){
  p_duration <- abs(duration2)/abs(duration)*w_duration
}
if(abs(duration)<=abs(duration2)){
  p_duration <- abs(duration)/abs(duration2)*w_duration
}

######### p_integration
if(abs(integration)>abs(integration2)){
  p_integration <- abs(integration2)/abs(integration)*w_integration
}
if(abs(integration)<=abs(integration2)){
  p_integration <- abs(integration)/abs(integration2)*w_integration
}

######### p_peak
if(abs(as.numeric(peak[2]))>abs(as.numeric(peak2[2]))){
  p_peak <- abs(as.numeric(peak2[2]))/abs(as.numeric(peak[2]))*w_peak
}
if(abs(as.numeric(peak[2]))<=abs(as.numeric(peak2[2]))){
  p_peak <- abs(as.numeric(peak[2]))/abs(as.numeric(peak2[2]))*w_peak
}

######## p_gospeed
if(abs(gospeed)>abs(gospeed2)){
  p_gospeed <- abs(gospeed2)/abs(gospeed)*w_gospeed
}
if(abs(gospeed)<=abs(gospeed2)){
  p_gospeed <- abs(gospeed)/abs(gospeed2)*w_gospeed
}

######## p_returnspeed
if(abs(returnspeed)>abs(returnspeed2)){
  p_returnspeed <- abs(returnspeed2)/abs(returnspeed)*w_returnspeed
}
if(abs(returnspeed)<=abs(returnspeed2)){
  p_returnspeed <- abs(returnspeed)/abs(returnspeed2)*w_returnspeed
}

# overall match possibility
possibility <- p_duration + p_baseline + ( abs(integration)/abs(integration2)*w_integration) + p_peak + p_gospeed + p_returnspeed



