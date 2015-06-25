setwd("E:/plot")

gap <- 0
current_pos <- 1
ratelimit <- 0.9
multiplier <- 200

datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )
dataset.pH1 <- datasetpH1[,3:4]
event.pH1 <- read.table("event.pH1.dat", col.names=c("MINUTES", "pH"))

eventchecked <- c(-1,0)


for(i in 1:9941){
  
  flag <- FALSE
  temp <- dataset.pH1[i:(i+59),1:2]
  
  
  for(j in 1:(dim(event.pH1)[1]/60)){
    x <- 0
    y <- 0
    z <- 0
    oneevent.pH1 <- event.pH1[((j-1)*60+1):(j*60),1:2]
    gap <<- temp[1,1] - oneevent.pH1[1,2]
    
    
    minevent <- min(oneevent.pH1[,2])
    mintemp <- min(temp[,1])
    if(minevent > mintemp){
      minbaseline <- mintemp
    }
    if(minevent <= mintemp){
      minbaseline <- minevent
    }
    
    mean.temp <- mean(temp[,1])
    mean.event <- mean(oneevent.pH1[,2])
    
    for(k in 1:60){
      
      x <- x + (temp[k,1]-mean.temp)^2
      y <- y + (oneevent.pH1[k,2]-mean.event)^2
      z <- z + (temp[k,1]-mean.temp)*(oneevent.pH1[k,2]-mean.event)
    }
    x <- sqrt(x)
    y <- sqrt(y)   
    rate <- abs(z/(x*y))
    
    r <- rate^2
    m <- c(i,j,x,y,z,r)
    print(m)
    #print(rate)
    #b <- c(i,j,rate)
    #print(b)
    if((!is.na(rate))&& (rate > ratelimit)){
      a <- c(i,j,rate)
      write.table(cbind(a[1],a[2],a[3]), "result.pH1.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
      if(!any(eventchecked == j)){
        eventchecked <<- rbind(eventchecked,j)
      }
    }
  }
}
write.table(eventchecked, "eventchecked.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
