#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel, Eric Lin 
#
#       File: start_sport.r
#
#       Function: upon start - extract previous num.points - 
#       This is so you can run alarms code which currently requires 241 points minimum      
#
#
#------------------------------------------------------------------------------------------



num.points <- 241

for (j in 1:n) {
  
  file <- paste("dataset_" , file.names[[name.i[j]]], ".dat", sep="")
  df.names <- c("Date", "Time", sensor.names.all[name.i[j]], "MINUTES")
  
  #dataset.previous <- read.table(file, col.names=df.names, sep=" ")
 # lenp <- countLines(file)  #countLines works 3times faster when files large
  
#  newdataset <- scan(file, '', skip = lenp-num.points, nlines = num.points, sep = '\n')
  
  
  
  dataset.previous <- read.table(file, col.names=df.names,sep="")
  set.df <- tail(dataset.previous, n=num.points)
  
  #remove NA rows
  row.has.na <- apply(set.df, 1, function(x){any(is.na(x))})
  sum(row.has.na)
  set.df <- set.df[!row.has.na,]
  
  
  
#  set.df <- matrix(NA, nrow=0, ncol=4)
#  for (i in 1:num.points) {
    
#    a <- unlist(strsplit(newdataset[i], ","))
#    newdata <- cbind(a[1], a[2], as.double(a[3]), as.numeric(a[4]))
#    set.df <- rbind(set.df, newdata)
#  }
  
  # dataset is our main data frame - do not delete or remove
  dataset[[j]] <- set.df
  colnames(dataset[[j]]) <- df.names
  
  # do some cleanup // note: moving to function auto cleans at return...(check this)
#  rm(newdataset, newdata, set.df, a, lenp, file, df.names)  
  
  
}

for (j in 1:n) {
  # plot data
  par(mar=c(1,1,1,1))
  plotting(dataset[[j]], 3, alarms[[j]], kPeriod,241, 241, label = sensor.names.all[name.i[j]]) 
  
}



