#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel 
#
#       File: start_sport.r
#
#       Function: upon start - extract previous num.points - 
#       This is so you can run alarms code which currently requires 241 points minimum      
#
#
#------------------------------------------------------------------------------------------

num.points <- 241

for(j in 1:n) {
  file <- paste("dataset_" , file.names[[name.i[j]]], ".dat", sep="")
  newdataset <- scan(file, '', skip = lenpH1-num.points, nlines = num.points, sep = '\n')

  set.df <- matrix(NA, nrow=0, ncol=4)
  

  for(i in 1:num.points){
  
    a <- unlist(strsplit(newdataset[i], " "))
    newdata <- cbind(a[1], a[2], as.double(a[3]),as.numeric(a[4]))
    set.df <- rbind(set.df, newdata)
  }

  dataset[[j]] <- cbind(as.data.frame(set.df[, 1:2]), as.double(set.df[, 3]),as.double(set.df[, 4]))
  colnames(dataset[[j]]) <- c("Date", "Time", sensor.names.all[name.i[j]], "MINUTES")

  plotting(dataset[[j]], 3, alarms[[j]], kPeriod,241, 241, label=sensor.names.all[name.i[j]])


}

