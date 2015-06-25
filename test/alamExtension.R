
################################################################################ 
#       alerts.pH1  updated
#
################################################################################

alerts.pH1 <- function(data_set_2, 
                       current_pos, 
                       wait, 
                       probS = c(.05, .95, .05, .95),
                       period.to.show = 720, 
                       bounds.pH = bounds.pH2) {
  
  ### IN THIS EXTENDED VERSION - adding a seaonal trend value +_ adjusting based on seasonal trend of data for daily 
  ## for this site

  
  if(current.code == 0){
    trendline.pH <- runquantile(data_set_2$pH, 241, probs=c(0.5, 0.75))
  }else {
    ## stop the baseline update
    dir.count <- max(countAB.pH1,countBW.pH1)
    data_pause <- c(data_set_2$pH[1:(current_pos-dir.count)],
                                rep(data_set_2$pH[(current_pos-dir.count)],dir.count))
    trendline.pH <- runquantile(data_pause, 241, probs=c(0.5, 0.75))
    
  }
 
  
  POINTS <- FALSE
  DIRECTION <- NULL 
  
  ## FOR ABOVE
  
  # data_set_2 <- data_set_2 + seasonalAdjust 
  
  if (data_set_2$pH[current_pos] >= trendline.pH[, 1][current_pos]) {
    countAB.pH1 <<- countAB.pH1 + 1
    countBW.pH1 <<- 0
    
    ### if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if(  (!is.na( mean(data_set_2$pH[(current_pos-countAB.pH1):current_pos]) )) &&
         (!is.na( max( data_set_2$pH[(current_pos-countAB.pH1):current_pos]) ) ) &&
         (!is.na( quantile(data_set_2$pH[(current_pos - 241):(current_pos - countAB.pH1)], probs = probS[4]))) 
      ){
      
      if ((countAB.pH1 >= wait[1]) && 
            (mean(data_set_2$pH[(current_pos-countAB.pH1):current_pos]) >= quantile(data_set_2$pH[(current_pos - 241):(current_pos - countAB.pH1)], probs = probS[4])) && 
            (max(data_set_2$pH[(current_pos-countAB.pH1):current_pos]) > bounds.pH[2])) {
        POINTS <- TRUE
        DIRECTION <- "AB" ##
      }
      
    }
  }
  
  ## FOR BELOW - + 
  
  if (data_set_2$pH[current_pos] < trendline.pH[, 1][current_pos]) {
    countAB.pH1 <<- 0
    countBW.pH1 <<- countBW.pH1 + 1
    
    ### if smaller sample mean is greater then previous 6 hours and point outside bounds then  ALARM
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
