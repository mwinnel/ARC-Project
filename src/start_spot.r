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
 
  dataset.previous <- read.table(file, col.names=df.names,sep="",fill=TRUE)
  set.df <- tail(dataset.previous, n=num.points)
  
  #remove NA rows
  row.has.na <- apply(set.df, 1, function(x){any(is.na(x))})
  sum(row.has.na)
  set.df <- set.df[!row.has.na,]
  
  dataset[[j]] <- set.df
  colnames(dataset[[j]]) <- df.names
  
  
}

#do some cleanup
rm(set.df,df.names,file,dataset.previous,row.has.na)


