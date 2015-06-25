



setwd("C:\Users\s2783343\Documents\ARC\data\sydney2 22.02.2013-17.09.2014")
setwd("C:/Users/s2783343/Documents/ARC/data/MELISSA DATA DOWNLOAD")


ph1 <- read.table("dataset_pH1.dat", header=FALSE)
colnames(ph1) <- c("Date", "Time", "pH1", "MINUTES")
temp <- read.table("dataset_TempC1.dat", header=FALSE)
colnames(temp) <- c("Date", "Time", "TempC1", "MINUTES")
cond <- read.table("dataset_Cond1.dat", header=FALSE)
colnames(cond) <- c("Date", "Time", "Cond1", "MINUTES")
turb <- read.table("dataset_TurbS1.dat", header=FALSE)
colnames(turb) <- c("Date", "Time", "TurbS1", "MINUTES")




ph1 <- ph1[(221918-100000):221918,]

temp <- temp[(221918-100000):221918,]

cond <- cond[(221918-100000):221918,]

turb <- turb[(221918-100000):221918,]





windows(width = 60, height = 40)   
par(mfcol = c(n,1))

plot(ph1$MINUTES,ph1$pH,typ="l", xlab="",ylab="pH1", axes = FALSE) 
#points(result.01.2days, col="blue")

axis(2)
box()
LabelTimeAxis()
title(main="Sydney Unit 3 Nov 2014 - Feb 2015",sub = "", cex.sub = 1.5, font.sub = 4, col.sub = "red")


plot(temp$MINUTES,temp$TempC1,typ="l", xlab="",ylab="TempC1", axes = FALSE) 
#points(result.01.2days, col="blue")

axis(2)
box()
LabelTimeAxis()


plot(cond$MINUTES,cond$Cond1,typ="l", xlab="",ylab="Cond1", axes = FALSE, ylim=c(0,10000) ) 
#points(result.01.2days, col="blue")

axis(2)
box()
LabelTimeAxis()

plot(turb$MINUTES,turb$TurbS1,typ="l", xlab="",ylab="TurbS1", axes = FALSE) 
#points(result.01.2days, col="blue")

axis(2)
box()
LabelTimeAxis()















windows(width = 60, height = 40)   

if( (n %% 2)==0 ) {
  par(mfcol = c(n/2,2))  
}else{
  par(mfcol = c(n,1))
}


start.minute <- read.table("minute.dat", header=FALSE)
start.minute<-0



if(start.minute == 0 & dim(dat)[1] > (2880*2) ) { 
  data.set <- dat[(dim(dat)[1]-(2880*2):dim(dat)[1],] 
} else {
  data.set <- dat[dat$MINUTES > start.minute[1,],] 
} 

if(dim(data.set)[1] <= 3)
{
  ##exit - nothing to update
  print("nothing to update")
  
}


## sub set
data.set <- dat[201133:(201133-2880),] 


for(j in 1:n) {
plotting(dataset[[j]], 3, alarms[[j]], kPeriod,241, 241, label=sensor.names.all[name.i[j]])
}





