datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )
datasetCond1 <- read.table("dataset_Cond1.dat", sep = " ")
datasetTempC1 <- read.table("dataset_TempC1.dat", sep = "\t")
datasetTurbS1 <- read.table("dataset_TurbS1.dat", sep = "\t")

lenpH1 <- dim(datasetpH1)[1]
lenCond1 <- dim(datasetCond1)[1]
lenTempC1 <- dim(datasetTempC1)[1]
lenTurbS1 <- dim(datasetTurbS1)[1]



## test if shorter than 241 points. if lenpH1 < 241  set to 1
if( (lenpH1 >= 241) ) { take.pH <- 241} else {take.pH <- lenpH1-1}
dataset.pH1 <- as.data.frame(datasetpH1[(lenpH1-take.pH):lenpH1,])
#colnames(dataset.pH1) <- c("Date", "Time", "pH", "MINUTES")

##do we need to do these steps? we could jump from dataset to extracting 241 tail points.

#newdatasetpH1 <- scan('dataset_pH1.dat', '', skip = lenpH1-241, nlines = 241, sep = '\n')
newdatasetCond1 <- scan('dataset_Cond1.dat', '', skip = lenCond1-241, nlines = 241, sep = '\n')
newdatasetTempC1 <- scan('dataset_TempC1.dat', '', skip = lenTempC1-241, nlines = 241, sep = '\n')
newdatasetTurbS1 <- scan('dataset_TurbS1.dat', '', skip = lenTurbS1-241, nlines = 241, sep = '\n')

#set.pH1 <- matrix(NA, nrow=0, ncol=3)
#for(i in 1:241){
#  a <- unlist(strsplit(newdatasetpH1[i], " "))
#  pH1data <- cbind(a[1], a[2], as.double(a[3]))
#  set.pH1 <- rbind(set.pH1, pH1data)
#}

#dataset.pH1 <- cbind(as.data.frame(set.pH1[, 1:2]), as.double(set.pH1[, 3]))
#dataset.pH1 <-cbind(dataset.pH1, MINUTES = HandleSuppliedTime2(dataset.pH1, "%d/%m/%Y", hourstart = 1, minstart = 4))

set.Cond1 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetCond1[i], " "))
  Cond1data <- cbind(a[1], a[2], as.double(a[3]))
  set.Cond1 <- rbind(set.Cond1, Cond1data)
}
dataset.Cond1 <- cbind(as.data.frame(set.Cond1[, 1:2]), as.double(set.Cond1[, 3]))
colnames(dataset.Cond1) <- c("Date", "Time", "Cond")
dataset.Cond1 <-cbind(dataset.Cond1, MINUTES = HandleSuppliedTime2(dataset.Cond1, "%d/%m/%Y", hourstart = 1, minstart = 4))

set.TempC1 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetTempC1[i], " "))
  TempC1data <- cbind(a[1], a[2], as.double(a[3]))
  set.TempC1 <- rbind(set.TempC1, TempC1data)
}
dataset.TempC1 <- cbind(as.data.frame(set.TempC1[, 1:2]), as.double(set.TempC1[, 3]))
colnames(dataset.TempC1) <- c("Date", "Time", "TempC")
dataset.TempC1 <-cbind(dataset.TempC1, MINUTES = HandleSuppliedTime2(dataset.TempC1, "%d/%m/%Y", hourstart = 1, minstart = 4))


set.TurbS1 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetTurbS1[i], " "))
  TurbS1data <- cbind(a[1], a[2], as.double(a[3]))
  set.TurbS1 <- rbind(set.TurbS1, TurbS1data)
}
dataset.TurbS1 <- cbind(as.data.frame(set.TurbS1[, 1:2]), as.double(set.TurbS1[, 3]))
colnames(dataset.TurbS1) <- c("Date", "Time", "TurbS")
dataset.TurbS1 <-cbind(dataset.TurbS1, MINUTES = HandleSuppliedTime2(dataset.TurbS1, "%d/%m/%Y", hourstart = 1, minstart = 4))

dataset.pH1[,4]

plotting(dataset.pH1, 3, alarms.pH1, kPeriod,241, 241, "pH1")
plotting(dataset.Cond1, 3, alarms.EC1, kPeriod,241, 241, "EC1")         
plotting(dataset.TurbS1, 3, alarms.TurbS1, kPeriod,241, 241, "TurbS1")
plotting(dataset.TempC1, 3, alarms.TempC1, kPeriod,241, 241,  "TempC1")

datasetpH2 <- read.table("dataset_pH2.dat", sep = "\t")
datasetCond2 <- read.table("dataset_Cond2.dat", sep = "\t")
datasetTempC2 <- read.table("dataset_TempC2.dat", sep = "\t")
datasetTurbS2 <- read.table("dataset_TurbS2.dat", sep = "\t")

lenpH2 <- dim(datasetpH2)[1]
lenCond2 <- dim(datasetCond2)[1]
lenTempC2 <- dim(datasetTempC2)[1]
lenTurbS2 <- dim(datasetTurbS2)[1]

newdatasetpH2 <- scan('dataset_pH2.dat', '', skip = lenpH2-241, nlines = 241, sep = '\n')
newdatasetCond2 <- scan('dataset_Cond2.dat', '', skip = lenCond2-241, nlines = 241, sep = '\n')
newdatasetTempC2 <- scan('dataset_TempC2.dat', '', skip = lenTempC2-241, nlines = 241, sep = '\n')
newdatasetTurbS2 <- scan('dataset_TurbS2.dat', '', skip = lenTurbS2-241, nlines = 241, sep = '\n')



set.pH2 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetpH2[i], " "))
  pH2data <- cbind(a[1], a[2], as.double(a[3]))
  set.pH2 <- rbind(set.pH2, pH2data)
}
dataset.pH2 <- cbind(as.data.frame(set.pH2[, 1:2]), as.double(set.pH2[, 3]))
colnames(dataset.pH2) <- c("Date", "Time", "pH")
dataset.pH2 <-cbind(dataset.pH2, MINUTES = HandleSuppliedTime2(dataset.pH2, "%d/%m/%Y", hourstart = 1, minstart = 4))

set.Cond2 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetCond2[i], " "))
  Cond2data <- cbind(a[1], a[2], as.double(a[3]))
  set.Cond2 <- rbind(set.Cond2, Cond2data)
}
dataset.Cond2 <- cbind(as.data.frame(set.Cond2[, 1:2]), as.double(set.Cond2[, 3]))
colnames(dataset.Cond2) <- c("Date", "Time", "Cond")
dataset.Cond2 <-cbind(dataset.Cond2, MINUTES = HandleSuppliedTime2(dataset.Cond2, "%d/%m/%Y", hourstart = 1, minstart = 4))

set.TempC2 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetTempC2[i], " "))
  TempC2data <- cbind(a[1], a[2], as.double(a[3]))
  set.TempC2 <- rbind(set.TempC2, TempC2data)
}
dataset.TempC2 <- cbind(as.data.frame(set.TempC2[, 1:2]), as.double(set.TempC2[, 3]))
colnames(dataset.TempC2) <- c("Date", "Time", "TempC")
dataset.TempC2 <-cbind(dataset.TempC2, MINUTES = HandleSuppliedTime2(dataset.TempC2, "%d/%m/%Y", hourstart = 1, minstart = 4))


set.TurbS2 <- matrix(NA, nrow=0, ncol=3)
for(i in 1:241){
  a <- unlist(strsplit(newdatasetTurbS2[i], " "))
  TurbS2data <- cbind(a[1], a[2], as.double(a[3]))
  set.TurbS2 <- rbind(set.TurbS2, TurbS2data)
}
dataset.TurbS2 <- cbind(as.data.frame(set.TurbS2[, 1:2]), as.double(set.TurbS2[, 3]))
colnames(dataset.TurbS2) <- c("Date", "Time", "TurbS")
dataset.TurbS2 <-cbind(dataset.TurbS2, MINUTES = HandleSuppliedTime2(dataset.TurbS2, "%d/%m/%Y", hourstart = 1, minstart = 4))




plotting(dataset.pH2, 3, alarms.pH2, kPeriod,241, lenpH2, "pH2")
plotting(dataset.Cond2, 3, alarms.EC2, kPeriod,241, lenCond2, "EC2") 
plotting(dataset.TurbS2, 3, alarms.TempC2, kPeriod,241, lenTempC2,  "TurbS2")
plotting(dataset.TempC2, 3, alarms.TurbS2, kPeriod,241, lenTurbS2,  "TempC2")
