#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Eric Hao 
#
#       File: Douglas Function.R
#
#       Function: Data Compression algorithm - follows the Douglas Algorithm
#
#
#------------------------------------------------------------------------------------------

library(data.table)



# Douglas algorithm - can this be written to not use global variables?
douglas <- function(PointList, epsilon) {
  
  dmax <- 0
  index <- 0
  end <- dim(PointList)[1]
  
  # if(k == stop.iter) return(TRUE)
  
  for (i in 2:(end-1)) {
    d <- distance(PointList[i,], PointList[1,], PointList[end,]) 
    if ( d > dmax ) {
      index <- i
      dmax <- d
    }
  }
  
  # If max distance is greater than epsilon, recursively simplify
  if ( dmax > epsilon ) 
  {
    # k <<- k+1
    
    # Recursive call 
    result <<- rbind(result, PointList[index,])
    douglas(PointList[1:index,], epsilon)
    douglas(PointList[index:end,], epsilon)
    
  } 
  else 
  {
    # ResultList <- rbind(PointList[1,], PointList[end,])
    return(TRUE)
  }
  
  # Return the result
}


distance <- function(point1,point2,point3) {
  A <- point2[,2]-point3[,2]
  B <- point3[,1]-point2[,1]
  C <- point2[,1]*point3[,2]-point3[,1]*point2[,2]
  h <- abs(A*point1[,1]+B*point1[,2]+C)/sqrt(A*A+B*B)
  return(h)
}


run.douglas <- function(data.set,epsil=0.01){

      dataD <- cbind(data.set[4],data.set[3])
      result <<- rbind(dataD[1,],dataD[dim(dataD)[1],])

      douglas(dataD,epsil)

      result <<- result[order(result$MINUTES),]

      ## extract the compressed data
      compressed.data <- data.set[data.set$MINUTES %in% result$MINUTES,]


      return(compressed.data)

}




