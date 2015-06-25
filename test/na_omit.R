setwd("C:/SentinelTest")

datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") );
datasetpH2 <- read.table("dataset_pH2.dat", col.names=c("Date", "Time", "pH", "MINUTES") );
datasetCond1 <- read.table("dataset_Cond1.dat", col.names=c("Date", "Time", "Cond", "MINUTES") );
datasetCond2 <- read.table("dataset_Cond2.dat", col.names=c("Date", "Time", "Cond", "MINUTES") );
datasetTurbA1 <- read.table("dataset_TurbA1.dat", col.names=c("Date", "Time", "Turb", "MINUTES") );
datasetTurbA2 <- read.table("dataset_TurbA2.dat", col.names=c("Date", "Time", "Turb", "MINUTES") );
datasetTurbS1 <- read.table("dataset_TurbS1.dat", col.names=c("Date", "Time", "Turb", "MINUTES") );
datasetTurbS2 <- read.table("dataset_TurbS2.dat", col.names=c("Date", "Time", "Turb", "MINUTES") );
datasetTempA1 <- read.table("dataset_TempA1.dat", col.names=c("Date", "Time", "Temp", "MINUTES") );
datasetTempC1 <- read.table("dataset_TempC1.dat", col.names=c("Date", "Time", "Temp", "MINUTES") );
datasetTempC2 <- read.table("dataset_TempC2.dat", col.names=c("Date", "Time", "Temp", "MINUTES") );

datasetpH1<-na.omit(datasetpH1)
datasetpH2<-na.omit(datasetpH2)
datasetCond1<-na.omit(datasetCond1)
datasetCond2<-na.omit(datasetCond2)
datasetTurbA1<-na.omit(datasetTurbA1)
datasetTurbA2<-na.omit(datasetTurbA2)
datasetTurbS1<-na.omit(datasetTurbS1)
datasetTurbS2<-na.omit(datasetTurbS2)
datasetTempA1<-na.omit(datasetTempA1)
datasetTempC1<-na.omit(datasetTempC1)
datasetTempC2<-na.omit(datasetTempC2)

write.table(datasetpH1, "dataset_pH1.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetpH2, "dataset_pH2.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetCond1, "dataset_Cond1.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetCond2, "dataset_Cond2.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTurbA1, "dataset_TurbA1.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTurbA2, "dataset_TurbA2.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTurbS1, "dataset_TurbS1.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTurbS2, "dataset_TurbS2.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTempA1, "dataset_TempA1.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTempC1, "dataset_TempC1.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
write.table(datasetTempC2, "dataset_TempC2.dat", row.names = FALSE, col.names = FALSE,quote = FALSE)
