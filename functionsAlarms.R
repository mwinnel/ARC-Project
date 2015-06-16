as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

##-------------------------------------------------------------------------
## mini functions
##-------------------------------------------------------------------------
Last <- function(x) {
  # Computes the last object of a given object.
  #
  # Args:
  #   x:Object to be calculated.
  # Retruns:
  #   The last object of the argument.
  tail(x, n = 1)
}

ToDecimal <- function(x) {
  # Convert from binary to decimal.
  #
  # Args:
  #   x: Binary number to be calculated.
  # Returns:
  #   The decimal from of x.
  sum(x * 2 ^ (rev(seq_along(x)) - 1)) 
}

SaveImage <- function(mins) {
  # Save currect graphic to a jpeg file.
  #
  # Args:
  #   mins: Currect time.
  # Returns:
  # 
  dev.print(jpeg, file = paste(mins, ".jpeg", sep=""), width = 1024, height = 768)
}

EarlyMorning <- function(x) {
  # Detect if it's early in the morning.
  #
  # Args:
  #   x: Time to be detected.
  #
  # Returns:
  #   TRUE: if it's between 2 a.m. and 6 a.m..
  #   FALSE: otherwise.
  x <- as.numeric(format(x, "%H"))
  if(x <= 06 && x >= 2) {
    return(TRUE) 
  }
  return(FALSE) 
}

### NOTE:  This might not be correct for Barrier 2 behaviour
PossibleLowFlow<- function(x, biny) { 
  
  EARLY <- EarlyMorning(Sys.time()) 
  ##test for DO.REDOX increase. 
  if(EARLY && (x == TempC.Redox.DO || x == Redox.DO || biny[2] == 1)) { 
    if(Last(alarms.Redox$DIRECTION) == "AB" && Last(alarms.DisOxy$DIRECTION) == "AB") {
      return(TRUE) 
    }
    if(Last(alarms.EC$DIRECTION) == "AB") {
      return(TRUE)
    }
  } 
  return(FALSE)
}


##-------------------------------------------------------------------------
##      SEND EMAIL  -- working code to embed an image
##-------------------------------------------------------------------------
### enclose this whole section into a function??  send some parameters to make strings for the mail??
################working code to embed an image
sendSMS <- function(mobile.num, subj) {
  require(rJython)
  rJython2 <- rJython()
  
  
  rJython2$exec( "import smtplib" )
  rJython2$exec("from email.MIMEText import MIMEText")
  
  smsMSG<-c(
    #Email settings
    "fromaddr = 'winnel@gmail.com'",
    paste("toaddrs =  '",mobile.num,"@directsms.com.au'",sep=""),
    paste("msgRoot = MIMEText('",subj,"')",sep=""),
    "msgRoot['Subject'] = ''",
    "msgRoot['From'] = fromaddr",
    "msgRoot['To'] = toaddrs",
    "username = 'winnel@gmail.com'",
    "password = ''",
    
    #Set SMTP server and send email, e.g., google mail SMTP server
    "server = smtplib.SMTP('smtp.gmail.com:587')",
    "server.ehlo()",
    "server.starttls()",
    "server.ehlo()",
    "server.login(username,password)",
    "server.sendmail(fromaddr, toaddrs, msgRoot.as_string())",
    "server.quit()")
  
  
  tryCatch({ jython.exec(rJython2,smsMSG) }, condition=function(ex) {
    Sys.sleep(5)
    tryCatch({ jython.exec(rJython2,smsMSG) }, condition=function(ex) {
      a <- print(ex)
      write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })
  })
}




sendEmail <- function(toaddr,fromaddr,imageName,subject,headerT) {
  #import smtplib
  require(rJython)
  rJython <- rJython()
  
  rJython$exec( "import smtplib" )
  rJython$exec("from email.MIMEText import MIMEText")
  ##rJython$exec("import base64")
  rJython$exec("from email.mime.image import MIMEImage")
  rJython$exec("from email.mime.multipart import MIMEMultipart" )
  
  #username <- trim(email)
  #ee <- ''
  
  mail<-c(
    #Email settings
    paste("fromaddr =  '",fromaddr,"'",sep=""),
    paste("toaddrs =  '",toaddr,"'",sep=""),
    paste("subject =  '",subject,"'",sep=""),
    
    "msgRoot = MIMEMultipart('related')",
    "msgRoot['Subject'] = subject",
    "msgRoot['From'] = fromaddr",
    "msgRoot['To'] = toaddrs",
    
    "msgRoot.preamble = 'This is a multi-part message in MIME format.'",
    "msgAlternative = MIMEMultipart('alternative')",
    "msgRoot.attach(msgAlternative) ",
    "msgText = MIMEText('This is the alternative plain text message.')",
    "msgAlternative.attach(msgText)",
    paste("msgText = MIMEText('<b><i>",headerT,"</i></b><img src=cid:image1>', 'html') ",sep=""),
    "msgAlternative.attach(msgText)",
    paste("fp = open('",imageName,".jpeg', 'rb')",sep=""),
    "msgImage = MIMEImage(fp.read())",
    "fp.close()",
    "msgImage.add_header('Content-ID', '<image1>')",
    "msgRoot.attach(msgImage)",
    
    #### NEED TO FIND NEW SERVER TO SEND THIS FROM
    "username = 'winnel@gmail.com'",
    "password = ''",
    
    #Set SMTP server and send email, e.g., google mail SMTP server
    
    "server = smtplib.SMTP('smtp.gmail.com:587')",
    "server.ehlo()",
    "server.starttls()",
    "server.ehlo()",
    "server.login(username,password)",
    "server.sendmail(fromaddr, toaddrs, msgRoot.as_string())",
    "server.quit()" )
  
  ##some how control the mail variables - make strings ??
  
  tryCatch({ jython.exec(rJython,mail) }, condition=function(ex) {
    Sys.sleep(5)
    tryCatch({ jython.exec(rJython,mail) }, condition=function(ex) {
      a <- print(ex)
      write(paste(Sys.time(),as.character(a),sep=" "), "log.txt",  append=TRUE); })
  })
}


##-------------------------------------------------------------------------
##      ALARMING INTELLIGENCE    -- new
##-------------------------------------------------------------------------
AlarmLogicTest <- function(current.minutes = 30, last.alarms = 0, current.code = 0, 
                           reporting.length.wait = 30, dataset.pH = 0, EMAIL = FALSE, SMS = FALSE) {   
  mins.since <- current.minutes - last.alarms      
  lastcode <- current.code  #store last code
  binary <- mins.since < 3  ## OVERLAP PERIOD
  current.c <- ToDecimal(binary)
  action <<- "NONE"    
  
  if (lastcode != current.c) {
    statechange.start.minutes <<- current.minutes 
  }
  
  if ((sum(binary) >= 2 || binary[1]) && (current.minutes - emailSENT) >= email.int.time) {
    lengthpH <- length(dataset.pH$pH)
    dif <- current.minutes-statechange.start.minutes       
    if (sum(binary) == 1 && 
          binary[1] &&
          sum(dataset.pH$pH[(lengthpH-dif):lengthpH] < ph.second.LOW) == 0 &&
          sum(dataset.pH$pH[(lengthpH-dif):lengthpH] > ph.second.HIGH) == 0) { 
      action <<- "SINGLE-PH"   
      print("SINGLE_PH")         
    }##checks PH outside second level bounds
    
    if (sum(binary) == 2 && binary[3]) { 
      action <<- "TURB-IGNORE"  #exit         
      return(current.c) 
    } 
    
    if (PossibleLowFlow(current.c, binary)) {
      action <<- "POSSIBLE-LOW-FLOW" #exit
      tex <- as.character(paste("Barrier",BarrierNO,"#S", SentinelNO, action,
                                as.character(format(Sys.time(), "%H:%M %b %d %Y"))))
      sendSMS(mobile.num=Melissa.M, tex)   
      sendSMS(mobile.num=Roger.M, tex) 
      sendSMS(mobile.num=Hujuin.M, tex) 
      emailSENT <<- current.minutes
      return(current.c)
    }
    
    if (EMAIL) {
      SaveImage(current.minutes)
      subject <- paste(subjectTEXT, SentinelNO)
      headerTEXT <- kHeader
      sendEmail(emailTO, emailFROM, current.minutes, subject, headerTEXT)
      action <<- "EMAIL"
      emailSENT <<- current.minutes
    }
    
    if (SMS) {  
      print(cat("SEND SMS -- ", variables[binary], "\n"))
      a <- paste(variables[binary], sep = "", collapse = " ")
      tex <- as.character(paste("Barrier", BarrierNO, "#S", SentinelNO, 
                                " Alarms detected in ", a, ".  :: ",
                                as.character(format(Sys.time(), "%H:%M %b %d %Y"))))
      sendSMS(mobile.num = Melissa.M, tex)   
      sendSMS(mobile.num = Roger.M, tex)
      sendSMS(mobile.num = Hujuin.M, tex)           
      emailSENT <<- current.minutes
      if(action == "NONE") {
        action <<- "SMS"
      } 
    }               
  }         
  return(current.c)
}



alerts <- function(data_set_2,  
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


