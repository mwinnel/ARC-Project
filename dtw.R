setwd("/Users/linhao/Dropbox/plot");
library(dtw)
library("scales")
datasetpH1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") );

##idx<-cbind(1:10000,datasetpH1[3])
##plot(idx,type="l")

#idx<-c(rep(7,900),datasetpH1[4700:5300,3],rep(7,100));
idx<-datasetpH1[4000:5500,3]+1
#idx<-c(datasetpH1[4700:5000,3],rep(7,1000));
#idx<-datasetpH1[3];
query<-datasetpH1[4700:5300,3]
#query<-datasetpH1[5000:5070,3]
#query<-rescale(query,to=c(5,7))
##idx<-seq(0,6.28,len=100);
##query<-sin(idx)+runif(100)/10;
## A cosine is for reference; sin and cos are offset by 25 samples
reference<-idx;
#plot(reference); lines(query,col="blue");
## Find the best match
alignmentOBE <-
  dtw(query,reference,
      keep=TRUE,step=rabinerJuangStepPattern(4, "c", FALSE),#step=asymmetric,
      open.end=TRUE,open.begin=TRUE);
#step=rabinerJuangStepPattern(4, "c", TRUE),
#plot(alignmentOBE,type="two",off=1);
dtwPlotTwoWay(alignmentOBE);

warpArea(alignmentOBE)


