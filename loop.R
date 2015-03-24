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


repeat {
  
  #-------------------------------------------------------------------------
  #      GET DATA 
  #------------------------------------------------------------------------- 
  
  dataset.mix <- GetData()
  n <- length(dataset.mix)

  for ( j in 1:n ) {
    
    if ( UPDATE[j] ) {
      # find the name of the sensor and get its spot number in the sensor names for this configuration
      # note: could also use a switch 
      
      
      #---------------------------------------------------------------------------------
      #      STORE DATA  -  ADD to SQL database here - currently storing to flat file
      #---------------------------------------------------------------------------------
      
      filename <- paste("dataset_",file.names[name.i[j]],".dat", sep="")  
      print(filename)         ## TEST: CHECK THESE MATCH CORRECTLY
      print(head(dataset[[j]]))
      
      
      write.table(dataset.mix[[j]], filename, row.names = FALSE, append = TRUE, col.names = FALSE)
      dataset[[j]] <- rbind(dataset[[j]], dataset.mix[[j]])
      
      #-------------------------------------------------------------------------
      #      PROCESS DATA  -- changed
      #-------------------------------------------------------------------------
      
      
      len <- dim(dataset[[j]])[1]
      
      if (len > 2) {
        
       
        if( !is.na( match(sensor.config[j],sensor.plot.process) ) ) {   ## Is this a sensor value we wish to plot and process for alarms?
          
          plotting(dataset[[j]], 3, alarms[[j]], 240, len, TRUE, label=sensor.config[j]) 
          
          dist <- paste("alerts." , file.names[[name.i[j]]], sep = "")
          print(dist)
        
          if (len > 241) {          # call alerts function  - need only dataset and length
            do.call( dist, list(dataset[[j]] , len )) 
          }
        }
        
      }

      
      # if length of data set greater than 2880 points - trim
      if (len > 2880) { dataset[[j]] <- tail(dataset[[j]], n = -1) }
      
      UPDATE[j] <- FALSE
      
    }
    
    
    
  }
  
  
  # CHECK : System Time - if maintenance due.
  # if(Sys.Time())
  

}



#-----------------------------------------------------------------------------
#      MULTI ALARMS ??   - not currently being used - may be used in future
#-----------------------------------------------------------------------------  
# last.alarms1 <- c(as.numeric(Last(alarms.pH1[,1])),as.numeric(Last(alarms.EC1[,1])), as.numeric(Last(alarms.TurbS1[,1])), as.numeric(Last(alarms.TempC1[,1]))) 
# minutes <- Last(dataset.pH1$MINUTES)
# current.sys.code <- AlarmLogicTest(minutes, last.alarms1, Last(system.codes1$Codes), reporting.length.wait=10, dataset.pH1, EMAIL=FALSE, SMS=TRUE) 
# system.codes1 <- rbind(system.codes1, c(minutes, current.sys.code, action))
# write.table(cbind(minutes, current.sys.code, action), "System_Codes1.dat", row.names=FALSE, append=TRUE, col.names=FALSE)

