#------------------------------------------------------------------------------------------
#       PROJECT: Griffith University ARC Linkage: WQAIS 
#       Author: Melissa Winnel, Eric Lin 
#
#       File: loop.R
#
#       Function: MAIN part of system. Gets data from socket
#                 - call functions and process / plot data 
#
#       IMPORTANT Notes:  Please edit with caution.
#
#
#------------------------------------------------------------------------------------------


key.sensor <-  match(sensor.config,sensor.plot.process) 


repeat {
  #-------------------------------------------------------------------------
  #      GET DATA 
  #------------------------------------------------------------------------- 
 
  ptm <- proc.time()
  dataset.mix <- GetData()
  
  # Stop the clock
 
  

  
  n <- length(dataset.mix)
  t <- proc.time() - ptm
  print(t)
  #-------------------------------------------------------------------------
  #      PROCESS DATA 
  #-------------------------------------------------------------------------   
  for ( j in 1:n ) {
    
    ptm2 <- proc.time()
    
    if ( UPDATE[j] ) {
      updatecounter <- updatecounter+1
      #---------------------------------------------------------------------------------
      #      STORE DATA  -  ADD to SQL database here - currently storing to flat file
      #---------------------------------------------------------------------------------
      
      filename <- paste("dataset_",file.names[name.i[j]],".dat", sep="")
      csvfilename <- paste("dataset_",file.names[name.i[j]],".csv", sep="")  
      write.table(dataset.mix[[j]], filename, row.names = FALSE, append = TRUE, col.names = FALSE)
      
      dataset[[j]] <- rbind(dataset[[j]], dataset.mix[[j]])
      
      #-------------------------------------------------------------------------
      #      PROCESS DATA  -- changed
      #-------------------------------------------------------------------------
      len <- dim(dataset[[j]])[1]
      
      if (len > 2) {
        
        if( !is.na(key.sensor[j]) ) {   ## Is this a sensor value we wish to plot and process for alarms?
          
          plotting(dataset[[j]], 3, alarms[[j]], 240, len, TRUE, label=sensor.config[j]) 
          
          
            
          if (len > 241) {          # call alerts function  - need only dataset and length
           
            
            dist <- paste("alerts." , file.names[[name.i[j]]], sep = "")
            print(dist)
            
           
            
            do.call( dist, list(dataset[[j]] , len )) 
            
       
            
          }
        }
        
      }
      
      
      # if length of data set greater than 2880 points - trim
      if (len > 2880) { dataset[[j]] <- tail(dataset[[j]], n = -1) }
      
      write.csv(tail(dataset[[j]], n = 1), file = csvfilename, row.names = FALSE) 
      csvname <- paste("dataset_",file.names[name.i[j]],".csv", sep="")
      
<<<<<<< HEAD
      tryCatch({ ftpUpload(csvname, paste(ftpAddress,csvname,sep="")) }, condition=function(ex) {
   
      a <- print(ex)
       write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })

      
      
      
=======

>>>>>>> f5e15856785f6de72a416f50517dec8bf0932deb
      
      # CHECK : System Time - if maintenance due. - every 3 hours for testing
      if(LastUPDATE_COUNT[j] == 180) {
        
        compressed <- run.douglas(tail(dataset[[j]]),1440)
        csvname2 <-  paste("douglas_",file.names[name.i[j]],".csv", sep="")
        write.csv(compressed, file = csvname2, row.names = FALSE, append = FALSE)
        ftpUpload(csvname2, paste(ftpAddress,tail(dataset[[j]]$MINUTES,n=1),csvname2,sep=""))
        LastUPDATE_COUNT[j]<-0
        
      }else{ 
        LastUPDATE_COUNT[j] <- LastUPDATE_COUNT[j]+1
        D_updatecounter <- D_updatecounter+1
      }
      
      UPDATE[j] <- FALSE
      
      #Sys.sleep(RealtimeInterval)
      # Stop the clock
      t2 <- proc.time() - ptm2
      print("LOOP")
      print(t2)
      
    Sys.sleep(2)
    }      
<<<<<<< HEAD
  }  

=======
  }
  if(updatecounter>=11){
    ToTgz(tgzName,files)
    tryCatch({ ftpUpload(tgzName, paste(ftpAddress,tgzName,sep="")) }, condition=function(ex) {
      Sys.sleep(5)
      tryCatch({ ftpUpload(tgzName, paste(ftpAddress,tgzName,sep=""))}, condition=function(ex) {
        a <- print(ex)
        write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })
    })
    updatecounter <- 0
  }
  if(D_updatecounter>=1440*11){
    ToTgz(D_tgzName,D_files)
    tryCatch({ ftpUpload(D_tgzName, paste(ftpAddress,D_tgzName,sep="")) }, condition=function(ex) {
      Sys.sleep(5)
      tryCatch({ ftpUpload(D_tgzName, paste(ftpAddress,D_tgzName,sep=""))}, condition=function(ex) {
        a <- print(ex)
        write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })
    })
    D_updatecounter <- 0
  }
  #Sys.sleep(RealtimeInterval)
>>>>>>> f5e15856785f6de72a416f50517dec8bf0932deb
  
}



#-----------------------------------------------------------------------------
#      MULTI ALARMS ??   - not currently being used - may be used in future
#-----------------------------------------------------------------------------  
# last.alarms1 <- c(as.numeric(Last(alarms.pH1[,1])),as.numeric(Last(alarms.EC1[,1])), as.numeric(Last(alarms.TurbS1[,1])), as.numeric(Last(alarms.TempC1[,1]))) 
# minutes <- Last(dataset.pH1$MINUTES)
# current.sys.code <- AlarmLogicTest(minutes, last.alarms1, Last(system.codes1$Codes), reporting.length.wait=10, dataset.pH1, EMAIL=FALSE, SMS=TRUE) 
# system.codes1 <- rbind(system.codes1, c(minutes, current.sys.code, action))
# write.table(cbind(minutes, current.sys.code, action), "System_Codes1.dat", row.names=FALSE, append=TRUE, col.names=FALSE)

