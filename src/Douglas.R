setwd("E:/plot")

epsilon <<- 0.3

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


distance <- function(point1,point2,point3)
{
  A <- point2[,2]-point3[,2]
  B <- point3[,1]-point2[,1]
  C <- point2[,1]*point3[,2]-point3[,1]*point2[,2]
  h <- abs(A*point1[,1]+B*point1[,2]+C)/sqrt(A*A+B*B)
  return(h)
}




datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )

data <- cbind(datasetpH1[4],datasetpH1[3])

#print (data[,2])

#h <- distance(data[1,],data[4598,],data[10000,])
#print(h)
result <<- rbind(data[1,],data[dim(data)[1],])

douglas(data,epsilon)
result <<- result[order(result$MINUTES),]

print(result)
write.table(result, "afterDouglas E = 0.3.dat", row.names = FALSE, append = TRUE, col.names = FALSE)