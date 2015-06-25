setwd("E:/plot")


datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") )

data <- cbind(datasetpH1[4],datasetpH1[3])

douglas <- read.table("afterDouglas E = 0.3.dat", col.names=c("MINUTES", "pH") )
douglas <- douglas[order(douglas$MINUTES),]

print(douglas)

#sort(douglas,decreasing = FALSE)


plot(data[,1], data[,2], type="l", col=1, lwd=3, lty=1, xlim=c(7259955,7269999))
box()

lines(douglas[,1], douglas[,2], col="green", cex=1.5)
box()