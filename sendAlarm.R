## ATTEMPT TO SEND TO SERVER


#for(j in 1:n) {  

  
#  len <- dim(dataset[[j]])[1]
 # csvname4 <- paste("dataset_alarm_",file.names[name.i[j]],".csv", sep="")
  
#  write.csv(dataset[[j]], file = csvname4, row.names = FALSE) 
  
  

#}


a_tgzName <- "alData.tgz"


#a_files <- list.files(pattern = "alarms.")
#a_files2 <- list.files(pattern = "dataset_alarm_")
a_files3 <- list.files(pattern = "System.Codes1.dat")
#f <- c(unlist(a_files),unlist(a_files2),unlist(a_files3))



ToTgz(a_tgzName,a_files3)


#tryCatch({ ftpUpload(a_tgzName, paste(ftpAddress,a_tgzName,sep=""))}, condition=function(ex) {
#  a <- print(ex)
#  write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE);
  #ftp failed so set to start.minute to do again next time
#  write.table(start.minute, file = "minute.txt", row.names = FALSE,col.names=FALSE)
  # after send delete files?
  
  
#})

 tryCatch({ system(paste("c:\\ARC\\pscp.exe -pw griffith c:\\ARC\\data\\",a_tgzName," ", nodeID,"@",Address,":data",sep=""), intern = TRUE)}, condition=function(ex) {
        a <- print(ex)
        write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })
        
        


