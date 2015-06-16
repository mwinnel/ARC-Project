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

  #dataset.mix <- GetData()  
  #dataset.mix <- dataTest()
 dataset.mix <- dataTestNew()
 
 for(i in 1:n){
   
   dataset.mix[[i]][[3]] <- as.numeric(as.character(dataset.mix[[i]][[3]]))
   dataset.mix[[i]][[4]] <- as.numeric(as.character(dataset.mix[[i]][[4]]))
   
 }
  
 # n <- length(dataset.mix)
 print(n)
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
      write.table(dataset.mix[[j]], filename, sep="," , row.names = FALSE, append = TRUE, col.names = FALSE)
      dataset[[j]] <- rbind(dataset[[j]], dataset.mix[[j]])
      lenNewData <- dim(dataset.mix[[j]])[1]
      
      #-------------------------------------------------------------------------
      #      PROCESS DATA  -- changed
      #-------------------------------------------------------------------------
      len <- dim(dataset[[j]])[1]
      
      if (len > 2) { 
        if( !is.na(key.sensor[j]) ) {   ## Is this a sensor value we wish to plot and process for alarms?
          al.x <- as.numeric(eval( parse( text = paste("alarms." , file.names[[name.i[j]]],"[,1]", sep = "")  )))
          al.y <- as.numeric(eval( parse( text = paste("alarms." , file.names[[name.i[j]]],"[,2]", sep = "")  )))
          plotting(dataset[[j]], 3,  al.x,al.y, 1840, len, TRUE, label=sensor.config[j]) 
           
          if (len > 241) {          # call alerts function  - need only dataset and length
            dist <- paste("alerts." , file.names[[name.i[j]]], sep = "")
            do.call( dist, list(dataset[[j]] , len, wait=waittimes[[j]] ))    
          }
        
        }else{
          print("NOT KEY SENSOR")
      }}      
      
      # if length of data set greater than 1500 points - trim.data <- default 720  
      if (len > trim.data) { dataset[[j]] <- tail(dataset[[j]], n = -1) }

      write.csv(tail(dataset[[j]], n=lenNewData ), file = csvfilename, row.names = FALSE) 
      csvname <- paste("dataset_",file.names[name.i[j]],".csv", sep="")
      # CHECK : System Time - if maintenance due.  -- moved this code to runDataCompression.R
    
      UPDATE[j] <- FALSE

    }      

  }  

 if(updatecounter>=11){
   #-----------------------------------------------------------------------------
   #      GET SYSTEM STATE CODE
   #-----------------------------------------------------------------------------  
   last.alarms1 <- c(as.numeric(Last(alarms.pH1[,1])),as.numeric(Last(alarms.Cond1[,1])), as.numeric(Last(alarms.TurbS1[,1])), as.numeric(Last(alarms.TempC1[,1]))) 
   last.sys.code <- Last(system.codes1$Codes)
   minutes <- Last(dataset$pH$MINUTES)
   current.sys.code <- AlarmLogicTest(minutes, last.alarms1, Last(system.codes1$Codes), reporting.length.wait=10, dataset$pH, EMAIL=FALSE, SMS=TRUE) 
   current.code <<- current.sys.code
 
   
   
   system.codes1 <- rbind(system.codes1, c(minutes, current.sys.code$code, action))
   
  if(current.sys.code$code != last.sys.code) {
      write.table(cbind(minutes, current.sys.code$code, action, current.sys.code$tex, dataset$pH$Date, dataset$pH$Time), "System_Codes1.dat", row.names=FALSE, append=TRUE, col.names=FALSE)
   
    if (last.sys.code > 0)
    {
      ## event just ended - should we assess impacts now??
      
      
      
    }
  }
  
  
  
  if (current.sys.code$code > 0)
  {
     ## ALARMS DETECTED
    # send data & alarms & text   
    source("sendAlarm.R")  
    
  }
  
  
  if( LIVESTREAM ) {
         
     ToTgz(tgzName,files)
     print("LIVE STREAM UPDATE")
       tryCatch({ ftpUpload(tgzName, paste("ftp://192.168.30.11/","alData.tgz",sep=""))}, condition=function(ex) {
          a <- print(ex)
          write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })
   }

 
    updatecounter <- 0   
  }

Sys.sleep(5)

}



