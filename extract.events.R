OEM <- c(4130:4700,10810:10920,14160:14260,15430:15650,36250:36740,37690:38100,60890:61040,64390:64520,65950:66050, 
         67860:67960,68850:69100,80830:81000,81610:81950,83020:83180,84480:84630,87375:87460,         
         88950:89150,90230:90430,90680:90780,95950:96050,104410:104580,111770:111850,113170:113250,116100:116380,         
         123100:123290,131820:131980,134630:134690)

INDEX.s <- c(4130,4700,10810,10920,14160,14260,15430,15650,36250,36740,37690,38100,60890,61040,64390,64520,65950,66050, 
         67860,67960,68850,69100,80830,81000,81610,81950,83020,83180,84480,84630,87375,87460,         
         88950,89150,90230,90430,90680,90780,95950,96050,104410,104580,111770,111850,113170,113250,116100,116380,         
         123100,123290,131820,131980,134630,134690)

library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con2 <- dbConnect(drv, host='192.168.30.10', port='5432', dbname='WQIAS', user='postgres', password='griffith24')
con <- dbConnect(drv, host='localhost', port='5432', dbname='WQIAS', user='postgres', password='griffith24')

pH1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_ph1")
tempc1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_tempc1")
turbs1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_turbs1")
cond1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_cond1")

pH2 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_ph2")
tempc2 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_tempc2")
turbs2 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_turbs2")

cond2 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_cond2")
tempa1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_tempa1")

turba1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_turba1")
turba2 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_turba2")


i <- 1
j <- 1
while(i < 54){

start <- INDEX.s[i]
end <- INDEX.s[i+1]
OEM2 <- c((start-1400):(end+50))
MINS <- pH1[(OEM2),4]

plot(pH1[!is.na(match(pH1$MINUTES,(MINS))) ,4],pH1[!is.na(match(pH1$MINUTES,(MINS))),3],type="l")
plot(cond1[!is.na(match(cond1$MINUTES,MINS)),4],cond1[!is.na(match(cond1$MINUTES,MINS)),3],type="l")
plot(tempc1[!is.na(match(tempc1$MINUTES,MINS)),4],tempc1[!is.na(match(tempc1$MINUTES,MINS)),3],type="l")
plot(turbs1[!is.na(match(turbs1$MINUTES,MINS)),4],turbs1[!is.na(match(turbs1$MINUTES,MINS)),3],type="l")
LabelTimeAxis()

syd2events <- as.data.frame(  cbind(  tempa1[!is.na(match(tempa1$MINUTES,MINS)),1:3],
                                      
                                      tempc1[!is.na(match(tempc1$MINUTES,MINS)),3],
                              
                                      tempc2[!is.na(match(tempc2$MINUTES,MINS)),3],
  
                                      pH1[!is.na(match(pH1$MINUTES,MINS)),3],
                                      pH2[!is.na(match(pH2$MINUTES,MINS)),3],
                                      
                                      cond1[!is.na(match(cond1$MINUTES,MINS)),3],
                                      cond2[!is.na(match(cond2$MINUTES,MINS)),3],
                                      
                                      
                                      turba1[!is.na(match(turbs1$MINUTES,MINS)),3],
                                      turba2[!is.na(match(turbs2$MINUTES,MINS)),3],
                                      
                                      turbs1[!is.na(match(turbs1$MINUTES,MINS)),3],
                                      turbs2[!is.na(match(turbs2$MINUTES,MINS)),3:4]) )


colnames(syd2events) <- c("Date","Time","TempA", "TempC", "TempC2",
                          "pH", "pH2", "Cond", "Cond2","TurbA", 
                          "TurbA2","TurbS", "TurbS2","MINUTES"  ) 

name2 <-  paste("syd2event", j, "<-","syd2events",sep="")
eval( parse(text=name2))

i <- i+2
j <- j+1

}


subset(mydata2, Age >= 20 & Age <= 30)


event.name <- paste("syd2event_", j , sep="")

ev <- paste( "dbWriteTable(con, c(\"events\", "  , event.name ,  "),", event.name  ,", row.names=FALSE, append=FALSE, overwrite=TRUE)")

dbWriteTable(con, c("events",  "syd2event_5" ), syd2event_5 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_6" ), syd2event_6 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_7" ), syd2event_7 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_8" ), syd2event_8 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_9" ), syd2event_9 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_10" ), syd2event_10 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_11" ), syd2event_11 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_12" ), syd2event_12 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_13" ), syd2event_13 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_25" ), syd2event_25 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_26" ), syd2event_26 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_27" ), syd2event_27 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_18" ), syd2event_18 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_19" ), syd2event_19 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_14" ), syd2event_14 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_15" ), syd2event_15 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_16" ), syd2event_16 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_17" ), syd2event_17 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_20" ), syd2event_25 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_21" ), syd2event_26 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_22" ), syd2event_27 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_23" ), syd2event_25 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con, c("events",  "syd2event_24" ), syd2event_26 , row.names=FALSE, append=FALSE, overwrite=TRUE)


dbWriteTable(con2, c("events",  "syd2event_5" ), syd2event_5 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_6" ), syd2event_6 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_7" ), syd2event_7 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_8" ), syd2event_8 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_9" ), syd2event_9 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_10" ), syd2event_10 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_11" ), syd2event_11 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_12" ), syd2event_12 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_13" ), syd2event_13 , row.names=FALSE, append=FALSE, overwrite=TRUE)

dbWriteTable(con2, c("events",  "syd2event_13" ), syd2event_13 , row.names=FALSE, append=FALSE, overwrite=TRUE)

dbWriteTable(con2, c("events",  "syd2event_25" ), syd2event_25 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_26" ), syd2event_26 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_27" ), syd2event_27 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_18" ), syd2event_18 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_19" ), syd2event_19 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_14" ), syd2event_14 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_15" ), syd2event_15 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_16" ), syd2event_16 , row.names=FALSE, append=FALSE, overwrite=TRUE)
dbWriteTable(con2, c("events",  "syd2event_17" ), syd2event_17 , row.names=FALSE, append=FALSE, overwrite=TRUE)



eval( parse(text=ev))



length(ex1 <- expression(1 + 0:9)) # 1
ex1
eval(ex1) # 1:10



OEMdayybefore <- OEM2-1440
OEMminsafter <- OEM2+20



OEM3 <- c(84480:84630,87375:87460,         
          88950:89150,90230:90430,90680:90780,95950:96050,104410:104580,111770:111850)

OEM4 <- c(111770:111850,113170:113250,116100:116380,         
          123100:123290,131820:131980,134630:134690)

#dataset.mix <- dataTest()

for(i in 1:n){
  
  pH1[[3]] <- as.numeric(as.character(pH1[[3]]))
  pH1[[4]] <- as.numeric(as.character(pH1[[4]]))
  
  cond1[[3]] <- as.numeric(as.character(cond1[[3]]))
  cond1[[4]] <- as.numeric(as.character(cond1[[4]]))
  
  tempc1[[3]] <- as.numeric(as.character(tempc1[[3]]))
  tempc1[[4]] <- as.numeric(as.character(tempc1[[4]]))
  
  turbs1[[3]] <- as.numeric(as.character(turbs1[[3]]))
  turbs1[[4]] <- as.numeric(as.character(turbs1[[4]]))
  
  pH2[[3]] <- as.numeric(as.character(pH2[[3]]))
  pH2[[4]] <- as.numeric(as.character(pH2[[4]]))
  
  cond2[[3]] <- as.numeric(as.character(cond2[[3]]))
  cond2[[4]] <- as.numeric(as.character(cond2[[4]]))
  
  tempc2[[3]] <- as.numeric(as.character(tempc2[[3]]))
  tempc2[[4]] <- as.numeric(as.character(tempc2[[4]]))
  
  turbs2[[3]] <- as.numeric(as.character(turbs2[[3]]))
  turbs2[[4]] <- as.numeric(as.character(turbs2[[4]]))
  
  
  
  
  
  dataset.mix[[i]][[4]] <- as.numeric(as.character(dataset.mix[[i]][[4]]))
  
}




MINS <- pH1[(OEM2),4]

MINS.lower <- pH1[OEMdayybefore,4]
MINS.upper <- pH1[OEMminsafter,4]
mindif <- diff(MINS)

MINS <- as.numeric(as.character(MINS))

tempc1[4130,1:3]
turbs1[4130,1:3]
cond1[4130,1:3]





pH <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_ph1")

windows(width = 60, height = 40)   
par(mfcol = c(4,1))

## write to database  --  store extra 


dataset <-  pH1[!is.na(match(pH1$MINUTES,(MINS))),]   

i <- 1
while(diff(MINS) < 3)
  {
  d <- split(f, drop=FALSE)
  vars <- diff(MINS) < 3
  
i <- i+1
}


plot(pH1[!is.na(match(pH1$MINUTES,(MINS))) ,4],pH1[!is.na(match(pH1$MINUTES,(MINS))),3],type="l")

plot(cond1[!is.na(match(cond1$MINUTES,MINS)),4],cond1[!is.na(match(cond1$MINUTES,MINS)),3],type="l")

plot(tempc1[!is.na(match(tempc1$MINUTES,MINS)),4],tempc1[!is.na(match(tempc1$MINUTES,MINS)),3],type="l")

plot(turbs1[!is.na(match(turbs1$MINUTES,MINS)),4],turbs1[!is.na(match(turbs1$MINUTES,MINS)),3],type="l")



plot(pH1[!is.na(match(pH1$MINUTES,(MINS.lower))) | !is.na(match(pH1$MINUTES,(MINS.upper)))  ,4],
     pH1[!is.na(match(pH1$MINUTES,(MINS.lower))) | !is.na(match(pH1$MINUTES,(MINS.upper))),3],type="l")

plot(cond1[!is.na(match(cond1$MINUTES,MINS)),4],cond1[!is.na(match(cond1$MINUTES,MINS)),3],type="l")

plot(tempc1[!is.na(match(tempc1$MINUTES,MINS)),4],tempc1[!is.na(match(tempc1$MINUTES,MINS)),3],type="l")

plot(turbs1[!is.na(match(turbs1$MINUTES,MINS)),4],turbs1[!is.na(match(turbs1$MINUTES,MINS)),3],type="l")





syd2events <- as.data.frame(cbind(pH1[!is.na(match(pH1$MINUTES,MINS)),1:3],
     cond1[!is.na(match(cond1$MINUTES,MINS)),3],tempc1[!is.na(match(tempc1$MINUTES,MINS)),3],
     turbs1[!is.na(match(turbs1$MINUTES,MINS)),3:4]))
colnames(syd2events) <- c("Date","Time","pH", "Cond","TempC", "TurbS","MINUTES") 





plot(dataset.tempc1[MINS,4],dataset.tempc1[MINS,3],type="l")


dbWriteTable(con2, c("events","impacts"), impacts, row.names=FALSE, append=FALSE, overwrite=TRUE)


event <- cbind(event.TempC[,1:3],event.DO[,3],event.Cond[,3],event.pH[,3],event.TurbS[1:3771,3],event.ORP[,3:4])



