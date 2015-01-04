setwd("/Users/linhao/Dropbox/plot");
library(data.table)
source("funcrtion")
epsilon<<-0.3

lastoneupdate<<-FALSE

dataset.TempC1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "Temp", "MINUTES") )


dataset.TempC1<-dataset.TempC1[1:300,]

repeat{
  if(!lastoneupdate){
    lastone<<-dataset.TempC1
    datatoDouglas<<-data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("Date", "Time", "Temp", "MINUTES"))), stringsAsFactors=F)
  }
  A<-data.table(dataset.TempC1, key="MINUTES")
  B<-data.table(lastone, key="MINUTES")
  C<-A[!B,allow.cartesian=TRUE]
  
  datatoDouglas<<-rbind(datatoDouglas, C)
  datatoDouglas<<-dataset.TempC1
  if(dim(datatoDouglas)[1] > 100){
    data <- cbind(datatoDouglas[4],datatoDouglas[3])
    
    #print (data[,2])
    
    #h <- distance(data[1,],data[4598,],data[10000,])
    #print(h)
    result <<- rbind(data[1,],data[dim(data)[1],])
    
    douglas(data,epsilon)
    result <<- result[order(result$MINUTES),]
    
    #print(result)
    
    plotting(result, 2, alarms.TempC1, kPeriod, dim(result)[1], TRUE, "TempC1")
    write.table(result, "TempAfterDouglas E = 0.3.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
    datatoDouglas<<-data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("Date", "Time", "Temp", "MINUTES"))), stringsAsFactors=F)
  }
  lastone<<-dataset.TempC1
  lastoneupdate<-TRUE
}


#######################################
#       douglas algorithm
####################################### 
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
    result <<- rbind(result, PointList[index,])
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

###############################################################
#   calculate the distance between points, used in douglas algorithm
###############################################################
distance <- function(point1,point2,point3)
{
  A <- point2[,2]-point3[,2]
  B <- point3[,1]-point2[,1]
  C <- point2[,1]*point3[,2]-point3[,1]*point2[,2]
  h <- abs(A*point1[,1]+B*point1[,2]+C)/sqrt(A*A+B*B)
  return(h)
}
