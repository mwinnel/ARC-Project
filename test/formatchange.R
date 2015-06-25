setwd("E:/plot")

gap <- 0
current_pos <- 1
ratelimit <- 0.9
multiplier <- 200

datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )

write.table(cbind(datasetpH1[4],datasetpH1[3]), "formated.pH1.dat", row.names = FALSE, append = TRUE, col.names = FALSE)


## change data formate - from 