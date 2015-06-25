### Three different benchmarking methods

# Method 1
ptm <- proc.time()

match(sensor.config,sensor.plot.process) 

apply(embed(dataset$pH,1),241,quantile,.75),

apply(embed(data_set_2$pH[1:999],241),1,quantile,.75)

proc.time() - ptm


#method 2
## testing different running median functions
rzoo <- function(x,n) rollapplyr(x, n, sd, fill=NA)
rttr <- function(x,n) runSD(x, n)
runq <- function(x,n) runquantile(x, n, probs=0.5)
library(rbenchmark)
set.seed(21)
x <- rnorm(1000)
all.equal(rzoo(x,250), rttr(x,250),runq(n,250))

benchmark(rzoo(x,250), rttr(x,250),runq(x,250))[,1:6]




#method 3

library(microbenchmark)
m <- microbenchmark( seq(10)^2, (1:10)^2, times=10000)




sensor.config <- c("TempA", "TempC", "pH", "Cond", "TurbS","TurbA",
                   "TempC2", "pH2", "Cond2", "TurbS2","TurbA2")   # order matters!!! effects how plots output - data order

j <- 3 
data_set_2 <- as.data.frame(dataset[[j]])
current_pos <-  dim(dataset[[j]])[1]
  
bench.q <- microbenchmark( runquantile(data_set_2$pH, 1, c(0.5,0.75)), 
                     apply(embed(data_set_2$pH[1:999],1),1,quantile,.75),
                    times=100)

#Unit: milliseconds
#expr      min       lq      mean   median        uq
#runquantile(data_set_2$pH, 241, c(0.5, 0.75)) 17.88679 18.15538  18.37821  18.2516  18.39127
#apply(embed(data_set_2$pH[1:999], 241), 1, quantile, 0.75) 96.10315 99.38805 105.71311 101.1350 105.08118
#max neval








################################################################################ 
#       alerts.pH1
#
################################################################################

alerts.pH1 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95), period.to.show = 720, bounds.pH = c(6.5, 7), wait=c(25, 20)) {

  
  trendline.pH = runquantile(data_set_2$pH, 241, probs=c(0.5, 0.75))
  #  apply(embed(x[1:10000],100),1,quantile,.75) 
  
  
  POINTS <- FALSE
  DIRECTION <- NULL ##
  
  if (data_set_2$pH[current_pos] >= trendline.pH[, 1][current_pos]) {
    countAB.pH1 <<- countAB.pH1 + 1
    countBW.pH1 <<- 0
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$pH[(current_pos-countAB.pH1):current_pos])))&&
         (!is.na(max(data_set_2$pH[(current_pos-countAB.pH1):current_pos])))&&
         (!is.na(quantile(data_set_2$pH[(current_pos - 241):(current_pos - countAB.pH1)], probs = probS[4])))) {
      if ((countAB.pH1 >= wait[1]) && 
            (mean(data_set_2$pH[(current_pos-countAB.pH1):current_pos]) >= quantile(data_set_2$pH[(current_pos - 241):(current_pos - countAB.pH1)], probs = probS[4])) && 
            (max(data_set_2$pH[(current_pos-countAB.pH1):current_pos]) > bounds.pH[2])) {
        POINTS <- TRUE
        DIRECTION <- "AB" ##
      }
    }
  }
  
  if (data_set_2$pH[current_pos] < trendline.pH[, 1][current_pos]) {
    countAB.pH1 <<- 0
    countBW.pH1 <<- countBW.pH1 + 1
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$pH[(current_pos-countBW.pH1):current_pos])))&&
         (!is.na(max(data_set_2$pH[(current_pos-countBW.pH1):current_pos])))&&
         (!is.na(quantile(data_set_2$pH[(current_pos - 241):(current_pos - countBW.pH1)], probs = probS[3])))) {
      if ((countBW.pH1 >= wait[2]) && 
            (mean(data_set_2$pH[(current_pos-countBW.pH1):current_pos]) <= quantile(data_set_2$pH[(current_pos - 241):(current_pos - countBW.pH1)], probs = probS[3])) && 
            (min(data_set_2$pH[(current_pos-countBW.pH1):current_pos]) < bounds.pH[1])) {
        
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  
  #### this section fix up for new alarms structure.
  if (POINTS) {
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$pH[current_pos], DIRECTION, max(countAB.pH1, countBW.pH1))
    alarms.pH1 <<- rbind(alarms.pH1, a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.pH1.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
  
}

