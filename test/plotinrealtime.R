setwd("/Users/linhao/Dropbox/plot");
library(data.table)




dataset.TempC1 <- read.table("dataset_pH1.dat", col.names=c("Date", "Time", "pH", "MINUTES") );data <- cbind(dataset.TempC1[4],dataset.TempC1[3])

A <- data.table(dataset.TempC1, key="MINUTES")
#B <- data.table(dataset.TempC1[1:1000,], key="MINUTES")
T <- data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("Date", "Time", "pH", "MINUTES"))), stringsAsFactors=F)
B <- data.table(T, key="MINUTES")
B<-A

A[!B]

plot(1:10)
dev.new
plot(1:20)
split.screen(figs = c( 1, 2 ) )
repeat{
  Sys.sleep(3)
erase.screen(1)
Sys.sleep(3)
screen(1)
plot(1:20,1:20)
Sys.sleep(3)
screen(2)
plot(1:10,1:10)
Sys.sleep(3)
erase.screen(2)
}


plot(1:10)
one<-dev.cur()          # this has aliases on different OSs
two<-dev.new()
plot(10:1)


firsttime<-TRUE
repeat
{
  if(firsttime)
  {
    plot(1:10)
    one<-dev.cur()
    dev.new()
    plot(1:20)
    two<-dev.cur()
    firsttime<-FALSE
  }
  if(dev.cur()==one)
  {
    plot(1:10)
  }
  else
  {
    dev.set(one)
    plot(1:10)
  }
  if(dev.cur()==two)
  {
    plot(1:20)
  }
  else
  {
    dev.set(two)
    plot(1:20)
  }
}

for(i in 1:10){
  plot(1:10,i:(9+i))
  Sys.sleep(1)
}

