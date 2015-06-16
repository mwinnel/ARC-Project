
summary(event)

for(i in 3:14){
  
  event[[i]] <- as.numeric(as.character(event[[i]]))
  
  
}

event.start <-eventfin$start.minute[eventfin$event_id == eventid] 
event.end <- eventfin$end.minute[eventfin$event_id == eventid] 


event <- event[event$MINUTES>(event.start-30),]
event <- event[event$MINUTES<=event.end+3,]



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
  
  if(colnames(result)[2] == "TurbS")
  {
    par(mar=c(3,4,1,2))
  }else{
    par(mar=c(0,4,1,2))
  }
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
