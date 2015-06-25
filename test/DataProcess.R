require(caTools)

#prime data
dataset_pH1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_ph1")
  
dataset_pH1[,3] <- as.numeric(as.character(dataset_pH1[,3]))
dataset_pH1[,4] <- as.numeric(as.character(dataset_pH1[,4]))
  
summary(dataset_pH1)

dataset_pH <- dataset_pH1



#115.7614  2019.619  0.6306261  0.3503668  7.806022  5.731776
#####real-time trendline build
highermark <- 0.6306261
lowermark <- 0.3503668
windowsize <- 2019.619
datalength <- dim(dataset_pH)[1]
upbounds<<-0
lowbounds<<-0
m_upbounds<<-1.2
m_lowbounds<<-0.8
c_upbounds<<-1.4
c_lowbounds<<-0.6
m_weight <<- 5
c_weight <<- 10
eventimpact<-115.7614
upthreshold <- 7.806022
lowthreshold <- 5.731776
i <- 1
markancher <- -1
trendline <- matrix(, nrow = datalength, ncol = 2)
eventmarks <- numeric(datalength)
trendlinestack <- dataset_pH$pH[1:windowsize]
trendline[1:windowsize,] <- runquantile(trendlinestack, windowsize, probs=c(lowermark, highermark))



while((i+windowsize)<=datalength){
  temp_trendline <- runquantile(trendlinestack, windowsize, probs=c(lowermark, highermark))
#####event detection process
  if((as.numeric(dataset_pH$pH[i+windowsize]) > trendline[i+windowsize-1,2]  || as.numeric(dataset_pH$pH[i+windowsize]) < trendline[i+windowsize-1,1]) && markancher < 0)
    markancher <- i+windowsize
  if(as.numeric(dataset_pH$pH[i+windowsize]) > trendline[i+windowsize-1,2] && as.numeric(dataset_pH$pH[i+windowsize]) > upthreshold){
    rate<-abs(as.numeric(dataset_pH$pH[i+windowsize])-trendline[i+windowsize-1,2])/trendline[i+windowsize-1,2]
    upbounds<-upbounds+10*rate
    lowbounds<-lowbounds-10*rate
    if(as.numeric(dataset_pH$pH[i+windowsize]) > m_upbounds*trendline[i+windowsize-1,2]){
      upbounds<-upbounds+m_weight
      lowbounds<-lowbounds-m_weight
    }
    if(as.numeric(dataset_pH$pH[i+windowsize]) > c_upbounds*trendline[i+windowsize-1,2]){
      upbounds<-upbounds+c_weight
      lowbounds<-lowbounds-c_weight
    }
    if(lowbounds<0)
      lowbounds<-0
  }
  else if(as.numeric(dataset_pH$pH[i+windowsize]) < trendline[i+windowsize-1,1] && as.numeric(dataset_pH$pH[i+windowsize]) < lowthreshold){
    rate<-abs(as.numeric(dataset_pH$pH[i+windowsize])-trendline[i+windowsize-1,1])/trendline[i+windowsize-1,1]
    lowbounds<-lowbounds+20*rate
    upbounds<-upbounds-20*rate    
    if(as.numeric(dataset_pH$pH[i+windowsize]) < m_lowbounds*trendline[i+windowsize-1,1]){
      upbounds<-upbounds-m_weight
      lowbounds<-lowbounds+m_weight
    }
    if(as.numeric(dataset_pH$pH[i+windowsize]) < c_lowbounds*trendline[i+windowsize-1,1]){
      upbounds<-upbounds-c_weight
      lowbounds<-lowbounds+c_weight
    }
    if(upbounds<0)
      upbounds<-0
  }
  else{
    lowbounds<-lowbounds-100
    upbounds<-upbounds-100
    
    if(lowbounds<0)
      lowbounds<-0
    if(upbounds<0)
      upbounds<-0
  }
  if(as.numeric(dataset_pH$pH[i+windowsize]) >= trendline[i+windowsize-1,1] && as.numeric(dataset_pH$pH[i+windowsize]) <= trendline[i+windowsize-1,2])
    markancher <- -1
    if(upbounds>=eventimpact){
    #eventmarks[i+windowsize] <- 1
    if(markancher>0)
      eventmarks[markancher:(i+windowsize)] <- 1
  }
  else if(lowbounds>=eventimpact){
    #eventmarks[i+windowsize] <- -1
    if(markancher>0)
      eventmarks[markancher:(i+windowsize)] <- -1
  }
  trendline[i+windowsize,] <- temp_trendline[windowsize,]
  if(eventmarks[i+windowsize]==0){
    trendlinestack[1:(windowsize-1)] <- trendlinestack[2:windowsize]
    trendlinestack[windowsize] <- dataset_pH$pH[i+windowsize]
  }
    
#  else
#    trendline[i+windowsize,] <- c(mean(as.numeric(temp_trendline[(1:windowsize),1])),mean(as.numeric(temp_trendline[(1:windowsize),2])))
  i <- i+1
}


plot(dataset_pH$MINUTES,dataset_pH$pH)
points(dataset_pH$MINUTES[eventmarks<0], dataset_pH$pH[eventmarks<0], col="yellow", lwd="0.2")
points(dataset_pH$MINUTES[eventmarks>0], dataset_pH$pH[eventmarks>0], col="green")

