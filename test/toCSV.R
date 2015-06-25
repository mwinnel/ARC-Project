setwd("E:/plot")


datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )
write.csv(datasetpH1, file = "dataset_pH1.csv", col.names = TRUE, row.names = FALSE)