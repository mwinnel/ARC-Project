UPDATE <- FALSE





repeat{
  ##-------------------------------------------------------------------------
  ##      GET DATA 
  ##------------------------------------------------------------------------- 
  dataset.mix <- GetData()

  ##-------------------------------------------------------------------------
  ##      STORE DATA
  ##-------------------------------------------------------------------------
  source("store.R")
  ##-------------------------------------------------------------------------
  ##      PROCESS DATA  -- changed
  ##-------------------------------------------------------------------------

  lenTurbS1 <- 0
  lenTempC1 <- 0
  lenpH1 <- 0
  lenCond1 <- 0
  
  lenTurbS2 <- 0
  lenTempC2 <- 0
  lenpH2 <- 0
  lenCond2 <- 0

  ##test for alerts and plot
  if(exists("dataset.TurbS1") && exists("dataset.TempC1")
     && exists("dataset.pH1") && exists("dataset.Cond1")) {
    lenTurbS1 <- dim(dataset.TurbS1)[1] 
    lenTempC1 <-  dim(dataset.TempC1)[1]
    lenpH1 <- dim(dataset.pH1)[1]
    lenCond1 <- dim(dataset.Cond1)[1]
  }
  
  if(exists("dataset.TurbS2") && exists("dataset.TempC2")
     && exists("dataset.pH2") && exists("dataset.Cond2")) {
    lenTurbS2 <- dim(dataset.TurbS2)[1] 
    lenTempC2 <-  dim(dataset.TempC2)[1]
    lenpH2 <- dim(dataset.pH2)[1]
    lenCond2 <- dim(dataset.Cond2)[1]
  }

  if(lenpH1 >= 241 && lenTempC1 >= 241 && lenCond1 >=241 && UPDATE) {    
    
    
        #alerts.pH1( dataset.pH1 , lenpH1, probS = probs.pH, period.to.show = period, bounds.pH)
        #alerts.EC1( dataset.Cond1 , lenCond1, probS = probs.EC, period.to.show = period, bounds.EC)
        #alerts.TurbS1( dataset.TurbS1 , lenTurbS1, probS = probs.TurbS, period.to.show = period, bounds.TurbS)
        #alerts.TempC1( dataset.TempC1 , lenTempC1, probS = probs.TempC, period.to.show = period, bounds.TempC, wait=c(30,30))    
    
    
        alerts.pH1( dataset.pH1 , lenpH1, period.to.show = period, bounds.pH)
        alerts.EC1( dataset.Cond1 , lenCond1, period.to.show = period, bounds.EC)
        alerts.TurbS1( dataset.TurbS1 , lenTurbS1,  period.to.show = period, bounds.TurbS)
        alerts.TempC1( dataset.TempC1 , lenTempC1,  period.to.show = period, bounds.TempC, wait=c(30,30))    
        
    
    ##-------------------------------------------------------------------------
    ##      MULTI ALARMS ??
    ##------------------------------------------------------------------------- 
    plotting(dataset.pH1, 3, alarms.pH1, kPeriod, lenpH1, TRUE, "pH1")
    plotting(dataset.Cond1, 3, alarms.EC1, kPeriod, lenCond1, TRUE, "EC1")         
    plotting(dataset.TurbS1, 3, alarms.TurbS1, kPeriod, lenTurbS1, TRUE, "TurbS1")
    plotting(dataset.TempC1, 3, alarms.TempC1, kPeriod, lenTempC1, TRUE, "TempC1")
    
    ##-------------------------------------------------------------------------
    ##      MULTI ALARMS ??
    ##-------------------------------------------------------------------------  
    last.alarms1 <- c(as.numeric(Last(alarms.pH1[,1])),
                     as.numeric(Last(alarms.EC1[,1])),
                     as.numeric(Last(alarms.TurbS1[,1])),
                     as.numeric(Last(alarms.TempC1[,1])))

    if(lenTurbS1 >= 2880) {  
      dataset.TurbS1 <- dataset.TurbS1[-(1), ]
    }
      
    if(lenpH1 >= 2880 &&  lenCond1 >= 2880 &&  lenTempC1 >= 2880) {  
      dataset.pH1 <- dataset.pH1[-(1), ]
      dataset.Cond1 <- dataset.Cond1[-(1), ]
      dataset.TempC1 <- dataset.TempC1[-(1), ]
    }
    
    minutes <- Last(dataset.pH1$MINUTES)
    current.sys.code <- AlarmLogicTest(minutes, last.alarms1, Last(system.codes1$Codes), reporting.length.wait=10, dataset.pH1, EMAIL=FALSE, SMS=TRUE) 
    system.codes1 <- rbind(system.codes1, c(minutes, current.sys.code, action))
    write.table(cbind(minutes, current.sys.code, action), "System_Codes1.dat", row.names=FALSE, append=TRUE, col.names=FALSE)
  }
  
  if(lenpH2 >= 241 && lenTempC2 >= 241 && lenCond2 >=241 && UPDATE) {    
    
    #alerts.pH2( dataset.pH1 , lenpH1, probS = probs.pH, period.to.show = period, bounds.pH)
    #alerts.EC2( dataset.Cond1 , lenCond1, probS = probs.EC, period.to.show = period, bounds.EC)
    #alerts.TurbS2( dataset.TurbS1 , lenTurbS1, probS = probs.TurbS, period.to.show = period, bounds.TurbS)
    #alerts.TempC2( dataset.TempC1 , lenTempC1, probS = probs.TempC, period.to.show = period, bounds.TempC, wait=c(30,30))    
    
    
    alerts.pH2( dataset.pH1 , lenpH1, period.to.show = period, bounds.pH)
    alerts.EC2( dataset.Cond1 , lenCond1,  period.to.show = period, bounds.EC)
    alerts.TurbS2( dataset.TurbS1 , lenTurbS1,period.to.show = period, bounds.TurbS)
    alerts.TempC2( dataset.TempC1 , lenTempC1,period.to.show = period, bounds.TempC, wait=c(30,30))    
    
    
    ##-------------------------------------------------------------------------
    ##      MULTI ALARMS ??
    ##------------------------------------------------------------------------- 
    
    plotting(dataset.pH2, 3, alarms.pH2, kPeriod, lenpH2, TRUE, "pH2")
    plotting(dataset.Cond2, 3, alarms.EC2, kPeriod, lenCond2, TRUE, "EC2")         
    plotting(dataset.TurbS2, 3, alarms.TurbS2, kPeriod, lenTurbS2, TRUE, "TurbS2")
    plotting(dataset.TempC2, 3, alarms.TempC2, kPeriod, lenTempC2, TRUE, "TempC2")
        
    ##-------------------------------------------------------------------------
    ##      MULTI ALARMS ??
    ##-------------------------------------------------------------------------  
    
    last.alarms2 <- c(as.numeric(Last(alarms.pH2[,1])),
                      as.numeric(Last(alarms.EC2[,1])),
                      as.numeric(Last(alarms.TurbS2[,1])),
                      as.numeric(Last(alarms.TempC2[,1])))
    
    
    
    if(lenTurbS2 >= 2880) {  
      dataset.TurbS2 <- dataset.TurbS2[-(1), ]
    }
    
    if(lenpH2 >= 2880 &&  lenCond2 >= 2880 &&  lenTempC2 >= 2880) {  
      dataset.pH2 <- dataset.pH2[-(1), ]
      dataset.Cond2 <- dataset.Cond2[-(1), ]
      dataset.TempC2 <- dataset.TempC2[-(1), ]
    }
    
    minutes <- Last(dataset.pH2$MINUTES)
    current.sys.code <- AlarmLogicTest(minutes, last.alarms2, Last(system.codes2$Codes), reporting.length.wait=10, dataset.pH2, EMAIL=FALSE, SMS=TRUE) 
    system.codes2 <- rbind(system.codes2, c(minutes, current.sys.code, action))
    write.table(cbind(minutes, current.sys.code, action), "System_Codes2.dat", row.names=FALSE, append=TRUE, col.names=FALSE)
  }
  
  UPDATE <- FALSE 
}