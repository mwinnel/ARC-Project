##########  PROJECT: SEQ Water Real Time Data Processing #######################
###### Author: Melissa Winnel
###### Functions for Project

kHeader <- "header text"

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

################################################################
# Initialisations required for calculations about time and dates
# Set up to handle dates between start of 1990 and end of 2019
kYears <- 1990:2020
years.leap <- ((kYears %% 4) == 0 & (kYears %% 100) != 0) | ((kYears %% 400) == 0)
dow <- c("Su","Mo","Tu","We","Th","Fr","Sa")
offset.days <- floor((kYears - 2000) * 365.25 + .75) # compared to 2000
YearBreaks <- offset.days * 24 * 60
YearBreakLabels <- kYears
YearLabels <- kYears[-length(kYears)]

mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
DaysInMonth.Year <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
# For Leap Years it is different
DaysInMonth.LeapYear <- DaysInMonth.Year; DaysInMonth.LeapYear[2] <- 29

Nyears <- length(YearLabels)
DaysInMonth <- NULL

for (i in 1:Nyears) {
  if(years.leap[i]) {
    DaysInMonth <- c(DaysInMonth, DaysInMonth.LeapYear)
  }
  else {
    DaysInMonth <- c(DaysInMonth, DaysInMonth.Year)
  }
}

MonthBreaks <- cumsum(c(YearBreaks[1],DaysInMonth*24*60))
MonthLabels <- factor(rep(mon,Nyears),levels=mon,ordered=TRUE)
MonthBreakLabels <- c(as.character(MonthLabels),mon[1])

DayLabels <- NULL
for (i in DaysInMonth) {
  DayLabels <- c(DayLabels, 1:i)
}
DayBreaks <- (0:length(DayLabels)) * 1440 + YearBreaks[1]
DayBreakLabels <- c(DayLabels, 1)

GetYMD <- function(Min) {
  # Calculate Year, Month, Day, DOW, TOD, Hour & Minute.
  #
  # Args:
  #   x: Min which is a vector of numbers of minutes from the start of 2000.
  # Returns:
  #   The Year, Month, Day, DOW, TOD, Hour & Minute in a data frame.
	TOD <- Min %% 1440
	hour <- TOD %/%60
	minute <- Min %% 60
	DOW <- factor(dow[1+ (floor(Min/1440)+6) %% 7],levels=dow,ordered=TRUE)
	Year<- cut(Min,YearBreaks,labels=YearLabels,right=FALSE) 
	Month <- MonthLabels[cut(Min,MonthBreaks,labels=FALSE,right=FALSE)]
	Day <- DayLabels[cut(Min,DayBreaks,labels=FALSE,right=FALSE)]
	data.frame(Year=Year,Month=Month,Day=Day,DOW=DOW,TOD=TOD,hour=hour,
		minute=minute)
}



################################################################################
#       HandleSuppliedTime3 : a function to turn the given time into 
#                             a number in secdons from the 1/01/2000 00:00:00
################################################################################

#HandleSuppliedTime3 <- function(dataset, date.fmt="%d/%m/%y %H:%M:%S")
#{
#   dateTime <- paste(as.character(dataset$Date),as.character(dataset$Time), sep=" ")
#   
#   date <- strptime(as.vector(dateTime), format=date.fmt)
#   date0 <- strptime("1/01/2000 00:00:00",format=date.fmt)
#	 
# Compute minutes relative to first record (ignoring daylight saving time)
#    date0 <- strptime("1/01/2000",format="%d/%m/%Y")
#    seconds.supplied <- round(as.numeric(difftime(date,date0,unit="secs")))
#    return(seconds.supplied)
# }
  
HandleSuppliedTime3 <- function(dataset, date.fmt, hourstart, minstart) {
  # Calculate supplied time from 2000/1/1.
  #
  # Args:
  #   dataset: data contain time information.
  #   date.fmt: date format in the given data.
  #   hourstart: hour start location in the given data.
  #   minstart: minute start location in the given data.
  # Returns:
  #   Supplied time, if cannot calculate supplied time, return 0.
  date <- strptime(dataset$Date, format=date.fmt)
  print(paste("Format for date: ", date))
  hour <- as.numeric(substring(dataset$Time, hourstart, hourstart+1))
  minute <- as.numeric(substring(dataset$Time, minstart, minstart+1))
  #print(paste("Format for date:",date, "hour: ", hour, "minute" , minute))

  # Compute minutes relative to first record (ignoring daylight saving time)
  date0 <- strptime("1/01/2000",format="%d/%m/%Y")
  minutes.supplied.local <- round(as.numeric(difftime(date, date0, unit="days"))) * 1440 + hour * 60 + minute
  
  # If there are records for which minutes.supplied could not be calculated,
  #      
  minutes.supplied.missing <- is.na(minutes.supplied.local)
  if (minutes.supplied.missing) {      
    return (as.numeric(0))
  }
  else {
    #print("minutes.supplied computed for all records", quote=FALSE)
    return(as.numeric(minutes.supplied.local))
  }
}


###############################################################################
##    LabelTimeAxisSec: converted the minutes version to deal with seconds
###          potential bug. losing seconds when do a divide by 60
###############################################################################

hadj.available <- getRversion() >= "2.2.0"

LabelTimeAxisSec <- function() {
  timerange <- par()$usr[1:2]   #gets the timerange in terms of MINUTES
  extent <- diff(timerange) / 60 #gets the difference between two values
  endtimes <- GetYMD(timerange / 60)   ##function to convert the minutes in timerange back to a date
  start.mins <- timerange[1] / 60
  end.mins <- timerange[2] / 60
  # When extent of time axis is sufficiently large that we only label years
  if(extent > 1000000) {
    if(hadj.available) {
      axis(1, at = YearBreaks, labels = YearBreakLabels, hadj = 0, tck=-.05, col = 1)
     }
    else {
      axis(1, at = YearBreaks, labels = paste(" ", YearBreakLabels), tck = -.05, col = 1)
    }
    axis(1,at=MonthBreaks,labels=FALSE,col=1)
  } 
  else {
    if (extent > 100000) {
      # When label months
      title(sub=as.character(endtimes$Year[1]),adj=0)
      title(sub=as.character(endtimes$Year[2]),adj=1)
      if (hadj.available) {
        axis(1, at = MonthBreaks, labels = MonthBreakLabels, hadj = 0, tck = -.035, col = 1)
      } 
      else {
        axis(1, at = MonthBreaks, labels = paste(" ", MonthBreakLabels), tck = -.035, col = 1)
      }
    } 
    else {
      if (extent > 10000) {
        # When label days (within months)
        title(sub=paste(endtimes$Month[1],endtimes$Year[1]),adj=0)
        title(sub=paste(endtimes$Month[2],endtimes$Year[2]),adj=1)
        if (hadj.available) {
          axis(1, at = DayBreaks, labels = DayBreakLabels, hadj = 0, tck = -.035, col = 1)
        } 
        else {
          axis(1, at = DayBreaks, labels = paste(" ", DayBreakLabels), tck = -.035, col = 1)
        }
      } 
      else {
        if(extent > 1000){
          # When label some hours (within days)
          title(sub = paste(endtimes$Day[1], endtimes$Month[1], endtimes$Year[1]), adj = 0)
          title(sub = paste(endtimes$Day[2], endtimes$Month[2], endtimes$Year[2]), adj = 1)
          axis(1, at = DayBreaks, labels = FALSE, tck = -.05)
          n <- length(DayBreaks)
          axis(1, at = DayBreaks + 720, labels = rep("12:00", n), tck = -.02, col = 1)
          axis(1, at = DayBreaks + 360, labels = rep("6:00", n), tck = -.03, col = 1)
          axis(1, at = DayBreaks + 1080, labels = rep("18:00", n), tck = -.02, col = 1)
        } 
        else {
          if (extent > 300) {
            # When label all hours (within days)
            title(sub = paste(endtimes$Day[1], endtimes$Month[1], endtimes$Year[1]), adj = 0)                      ##paste at start of graph
            title(sub = paste(endtimes$Day[2], endtimes$Month[2], endtimes$Year[2]), adj = 1)                   ##paste at end of graph
            axis(1,at=DayBreaks,labels=FALSE,tck=-.07,col=1)
            base <- start.mins - start.mins %% 1440
            axis(1, at = (base * 60) + (60 * 60) * (0:47), labels = paste(rep(0:23,2), "00", sep=":"), tck = -.02, col = 1)
          } 
          else {
            # When label every 10 minutes
            title(sub = paste(endtimes$Day[1],endtimes$Month[1], endtimes$Year[1]), adj = 0)
            title(sub = paste(endtimes$Day[2],endtimes$Month[2], endtimes$Year[2]), adj = 1)
            base <- start.mins - start.mins %% 1440
            axis(1, at = base + 60 * (0:47), labels = FALSE, tck = -.03, col = 1)
            axis(1, at = base + 10 * (0:287), labels = paste(rep(rep(0:23, each = 6), 2), ":",
                 rep(0:5,48), "0", sep = ""), tck = -.01, col = 1)
          }
        }
      }
    }
  }
}

LabelTimeAxis <- function() {
  timerange <- par()$usr[1:2]
  extent <- diff(timerange)
  endtimes <- GetYMD(timerange)
  start.mins <- timerange[1]
  end.mins <- timerange[2]
  # When extent of time axis is sufficiently large that we only label years
  if (extent > 1000000) {
    if (hadj.available) {
      axis(1, at = YearBreaks, labels = YearBreakLabels, hadj=0, tck = -.05, col = 1)
    }
    else {
      axis(1, at = YearBreaks, labels = paste(" ", YearBreakLabels), tck = -.05, col = 1)
    }
    axis(1, at = MonthBreaks, labels = FALSE, col = 1)
  } 
  else {
    if (extent > 100000){
      # When label months
      title(sub = as.character(endtimes$Year[1]), adj = 0)
      title(sub = as.character(endtimes$Year[2]), adj = 1)
      if (hadj.available){
        axis(1, at = MonthBreaks, labels = MonthBreakLabels, hadj = 0, tck = -.035, col = 1)
      } 
      else {
        axis(1, at = MonthBreaks, labels = paste(" ", MonthBreakLabels), tck = -.035, col = 1)
      }
    } 
    else {
      if (extent > 10000) {
        # When label days (within months)
        title(sub = paste(endtimes$Month[1], endtimes$Year[1]), adj=0)
        title(sub = paste(endtimes$Month[2], endtimes$Year[2]), adj=1)
        if (hadj.available){
          axis(1, at = DayBreaks, labels = DayBreakLabels, hadj = 0, tck = -.035, col = 1)
        } 
        else {
          axis(1, at = DayBreaks, labels = paste(" ", DayBreakLabels), tck = -.035, col = 1)
        }
      }    
      else {
        if (extent > 1000) {
          # When label some hours (within days)
          title(sub = paste(endtimes$Day[1], endtimes$Month[1], endtimes$Year[1]), adj = 0)
          title(sub = paste(endtimes$Day[2], endtimes$Month[2], endtimes$Year[2]), adj = 1)
          axis(1, at = DayBreaks, labels = FALSE, tck = -.05)
          n <- length(DayBreaks)
          axis(1, at = DayBreaks + 720, labels = rep("12:00", n), tck = -.02, col = 1)
          axis(1, at = DayBreaks + 360, labels = rep("6:00", n), tck = -.03, col = 1)
          axis(1, at = DayBreaks + 1080, labels = rep("18:00", n), tck = -.02, col = 1)
        } 
        else {
          if (extent > 300){
            # When label all hours (within days)
            title(sub = paste(endtimes$Day[1], endtimes$Month[1], endtimes$Year[1]), adj = 0)
            title(sub = paste(endtimes$Day[2], endtimes$Month[2], endtimes$Year[2]), adj = 1)
            axis(1, at = DayBreaks, labels = FALSE, tck = -.07, col = 1)
            base <- start.mins - start.mins %% 1440
            axis(1, at = base + 60 * (0:47), labels = paste(rep(0:23,2),"00",sep=":"), tck = -.02, col = 1)
          } 
          else {
            # When label every 10 minutes
            title(sub = paste(endtimes$Day[1], endtimes$Month[1], endtimes$Year[1]), adj = 0)
            title(sub = paste(endtimes$Day[2], endtimes$Month[2], endtimes$Year[2]), adj = 1)
            base <- start.mins - start.mins %% 1440
            axis(1,at=base + 60 * (0:47), labels = FALSE, tck = -.03, col = 1)
            axis(1,at=base + 10 * (0:287), labels = paste(rep(rep(0:23, each = 6), 2), ":",
                 rep(0:5,48), "0", sep = ""), tck = -.01, col = 1)
          }
        }
      }
    }
  }
}


################################################################################ 
#       alerts.pH1
#
################################################################################

alerts.pH1 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95),
         period.to.show = 720, bounds.pH = c(6.5, 7), wait=c(25, 20)) {
  
 # data_set_2 <- dataset[[2]]
  print("IN ALERTS &&&&&&&&&&&&&&&&&&&&&&&&")
  trendline.pH = runquantile(data_set_2$pH, 241, probs=c(0.5, 0.75))
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


################################################################################
#       alerts.pH2
#
################################################################################
alerts.pH2 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95),
                       period.to.show = 720, bounds.pH = c(6.5, 7), wait=c(25, 20)) {
  trendline.pH = runquantile(data_set_2$pH, 241, probs=c(0.5, 0.75))
  POINTS <- FALSE
  DIRECTION <- NULL ##
  
  if (data_set_2$pH[current_pos] >= trendline.pH[, 1][current_pos]) {
    countAB.pH2 <<- countAB.pH2 + 1
    countBW.pH2 <<- 0
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$pH[(current_pos-countAB.pH2):current_pos])))&&
         (!is.na(max(data_set_2$pH[(current_pos-countAB.pH2):current_pos])))&&
         (!is.na(quantile(data_set_2$pH[(current_pos - 241):(current_pos - countAB.pH2)], probs = probS[4])))) {
      if ((countAB.pH2 >= wait[1]) && 
            (mean(data_set_2$pH[(current_pos-countAB.pH2):current_pos]) >= quantile(data_set_2$pH[(current_pos - 241):(current_pos - countAB.pH2)], probs = probS[4])) && 
            (max(data_set_2$pH[(current_pos-countAB.pH2):current_pos]) > bounds.pH[2])) {
        POINTS <- TRUE
        DIRECTION <- "AB" ##
      }
    }
  }
  
  if (data_set_2$pH[current_pos] < trendline.pH[, 1][current_pos]) {
    countAB.pH2 <<- 0
    countBW.pH2 <<- countBW.pH2 + 1
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$pH[(current_pos-countBW.pH2):current_pos])))&&
         (!is.na(max(data_set_2$pH[(current_pos-countBW.pH2):current_pos])))&&
         (!is.na(quantile(data_set_2$pH[(current_pos - 241):(current_pos - countBW.pH2)], probs = probS[3])))) {
      if ((countBW.pH2 >= wait[2]) && 
            (mean(data_set_2$pH[(current_pos-countBW.pH2):current_pos]) <= quantile(data_set_2$pH[(current_pos - 241):(current_pos - countBW.pH2)], probs = probS[3])) && 
            (min(data_set_2$pH[(current_pos-countBW.pH2):current_pos]) < bounds.pH[1])) {
      
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if (POINTS) {
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$pH[current_pos], DIRECTION, max(countAB.pH2, countBW.pH2))
    alarms.pH2 <<- rbind(alarms.pH2, a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.pH2.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


################################################################################
#       alerts.TurbS1
#
################################################################################
alerts.TurbS1 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95), 
                          period.to.show = 720, bounds.TurbS = c(6.5, 7), wait = c(20, 20)) {
  
  trendline.TurbS = runquantile(data_set_2$TurbS, 241, probs=c(0.5, 0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if (data_set_2$TurbS[current_pos] >= trendline.TurbS[, 1][current_pos]) {
    countAB.TurbS1 <<- countAB.TurbS1 + 1
    countBW.TurbS1 <<- 0
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TurbS[(current_pos-countAB.TurbS1):current_pos])))&&
         (!is.na(max(data_set_2$TurbS[(current_pos-countAB.TurbS1):current_pos])))&&
         (!is.na(quantile(data_set_2$TurbS[(current_pos - 241):(current_pos - countAB.TurbS1)], probs = probS[4])))){
      if((countAB.TurbS1 >= wait[1]) && 
          (mean(data_set_2$TurbS[(current_pos-countAB.TurbS1):current_pos]) >= quantile(data_set_2$TurbS[(current_pos - 241):(current_pos - countAB.TurbS1)], probs = probS[4])) &&
          (max(data_set_2$TurbS[(current_pos-countAB.TurbS1):current_pos]) > bounds.TurbS[2])) { 
      
        POINTS <- TRUE
        DIRECTION <- "AB"
      }
    }
  }
  
  if(data_set_2$TurbS[current_pos] < trendline.TurbS[,1][current_pos]) {
    countAB.TurbS1 <<- 0
    countBW.TurbS1 <<- countBW.TurbS1 + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TurbS[(current_pos-countBW.TurbS1):current_pos])))&&
         (!is.na(max(data_set_2$TurbS[(current_pos-countBW.TurbS1):current_pos])))&&
         (!is.na(quantile(data_set_2$TurbS[(current_pos - 241):(current_pos - countBW.TurbS1)], probs = probS[3])))){
      if((countBW.TurbS1 >= wait[2]) &&
          (mean(data_set_2$TurbS[(current_pos - countBW.TurbS1):current_pos]) <= quantile(data_set_2$TurbS[(current_pos - 241):(current_pos-countBW.TurbS1)], probs = probS[3])) &&
          (min(data_set_2$TurbS[(current_pos-countBW.TurbS1):current_pos]) < bounds.TurbS[1])) {
      
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if(POINTS) {
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$TurbS[current_pos], DIRECTION, max(countAB.TurbS1, countBW.TurbS1))
    alarms.TurbS1 <<- rbind(alarms.TurbS1,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.TurbS1.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }     
  #return(trendline.TurbS[,1])
}



################################################################################
#       alerts.TurbS2
#
################################################################################
alerts.TurbS2 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95), 
                         period.to.show = 720, bounds.TurbS = c(6.5, 7), wait = c(20, 20)) {
  
  trendline.TurbS = runquantile(data_set_2$TurbS, 241, probs=c(0.5, 0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if (data_set_2$TurbS[current_pos] >= trendline.TurbS[, 1][current_pos]) {
    countAB.TurbS2 <<- countAB.TurbS2 + 1
    countBW.TurbS2 <<- 0
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TurbS[(current_pos-countAB.TurbS2):current_pos])))&&
         (!is.na(max(data_set_2$TurbS[(current_pos-countAB.TurbS2):current_pos])))&&
         (!is.na(quantile(data_set_2$TurbS[(current_pos - 241):(current_pos - countAB.TurbS2)], probs = probS[4])))){
      if((countAB.TurbS2 >= wait[1]) && 
          (mean(data_set_2$TurbS[(current_pos-countAB.TurbS2):current_pos]) >= quantile(data_set_2$TurbS[(current_pos - 241):(current_pos - countAB.TurbS2)], probs = probS[4])) &&
          (max(data_set_2$TurbS[(current_pos-countAB.TurbS2):current_pos]) > bounds.TurbS[2])) { 
      
        POINTS <- TRUE
        DIRECTION <- "AB"
      }
    }
  }
  
  if(data_set_2$TurbS[current_pos] < trendline.TurbS[,1][current_pos]) {
    countAB.TurbS2 <<- 0
    countBW.TurbS2 <<- countBW.TurbS2 + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TurbS[(current_pos-countBW.TurbS2):current_pos])))&&
         (!is.na(max(data_set_2$TurbS[(current_pos-countBW.TurbS2):current_pos])))&&
         (!is.na(quantile(data_set_2$TurbS[(current_pos - 241):(current_pos - countBW.TurbS2)], probs = probS[3])))){
      if((countBW.TurbS2 >= wait[2]) &&
          (mean(data_set_2$TurbS[(current_pos - countBW.TurbS2):current_pos]) <= quantile(data_set_2$TurbS[(current_pos - 241):(current_pos-countBW.TurbS2)], probs = probS[3])) &&
          (min(data_set_2$TurbS[(current_pos-countBW.TurbS2):current_pos]) < bounds.TurbS[1])) {
      
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if(POINTS) {
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$TurbS[current_pos], DIRECTION, max(countAB.TurbS2, countBW.TurbS2))
    alarms.TurbS2 <<- rbind(alarms.TurbS2,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.TurbS2.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }     
  #return(trendline.TurbS[,1])
}


################################################################################
#       alerts.TempC1
#
################################################################################
alerts.TempC1 <- function(data_set_2, current_pos, probS = c(.05,.95,.05,.95),
                          period.to.show = 720, bounds.TempC = c(6.5,7), wait = c(20,20)){
 
  print("HERE")
  trendline.TempC = runquantile(data_set_2$TempC, 241, probs = c(0.5,0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  print("HERE 2")
  if (data_set_2$TempC[current_pos] >= trendline.TempC[, 1][current_pos]) {
    countAB.TempC1 <<- countAB.TempC1 + 1
    countBW.TempC1 <<- 0
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TempC[(current_pos-countAB.TempC1):current_pos])))&&
         (!is.na(max(data_set_2$TempC[(current_pos-countAB.TempC1):current_pos])))&&
         (!is.na(quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countAB.TempC1)], probs = probS[4])))){
      if ((countAB.TempC1 >= wait[1]) && 
            (mean(data_set_2$TempC[(current_pos-countAB.TempC1):current_pos]) >= quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countAB.TempC1)], probs = probS[4])) && 
            (max(data_set_2$TempC[(current_pos-countAB.TempC1):current_pos]) > bounds.TempC[2])) {
      
        POINTS <- TRUE
        DIRECTION <- "AB"
      }
    }
  }
  
  if (data_set_2$TempC[current_pos] < trendline.TempC[, 1][current_pos]) {
    countAB.TempC1 <<- 0
    countBW.TempC1 <<- countBW.TempC1 + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TempC[(current_pos-countBW.TempC1):current_pos])))&&
         (!is.na(max(data_set_2$TempC[(current_pos-countBW.TempC1):current_pos])))&&
         (!is.na(quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countBW.TempC1)], probs = probS[3])))){
      if ((countBW.TempC1 >= wait[2]) &&
            (mean(data_set_2$TempC[(current_pos-countBW.TempC1):current_pos]) <= quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countBW.TempC1)], probs = probS[3])) && 
            (min(data_set_2$TempC[(current_pos-countBW.TempC1):current_pos]) < bounds.TempC[1])) {
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if (POINTS) {
    ##store points - add show history flag
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$TempC[current_pos], DIRECTION, max(countAB.TempC1, countBW.TempC1))
    alarms.TempC1 <<- rbind(alarms.TempC1,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.TempC1.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


################################################################################
#       alerts.TempC2
#
################################################################################
alerts.TempC2 <- function(data_set_2, current_pos, probS = c(.05,.95,.05,.95),
                         period.to.show = 720, bounds.TempC = c(6.5,7), wait = c(20,20)){
  
  trendline.TempC = runquantile(data_set_2$TempC, 241, probs = c(0.5,0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if (data_set_2$TempC[current_pos] >= trendline.TempC[, 1][current_pos]) {
    countAB.TempC2 <<- countAB.TempC2 + 1
    countBW.TempC2 <<- 0
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TempC[(current_pos-countAB.TempC2):current_pos])))&&
         (!is.na(max(data_set_2$TempC[(current_pos-countAB.TempC2):current_pos])))&&
         (!is.na(quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countAB.TempC2)], probs = probS[4])))){
      if ((countAB.TempC2 >= wait[1]) && 
            (mean(data_set_2$TempC[(current_pos-countAB.TempC2):current_pos]) >= quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countAB.TempC2)], probs = probS[4])) && 
            (max(data_set_2$TempC[(current_pos-countAB.TempC2):current_pos]) > bounds.TempC[2])) {
      
        POINTS <- TRUE
        DIRECTION <- "AB"
      }
    }
  }
  
  if (data_set_2$TempC[current_pos] < trendline.TempC[, 1][current_pos]) {
    countAB.TempC2 <<- 0
    countBW.TempC2 <<- countBW.TempC2 + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$TempC[(current_pos-countBW.TempC2):current_pos])))&&
         (!is.na(max(data_set_2$TempC[(current_pos-countBW.TempC2):current_pos])))&&
         (!is.na(quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countBW.TempC2)], probs = probS[3])))){
      if ((countBW.TempC2 >= wait[2]) &&
            (mean(data_set_2$TempC[(current_pos-countBW.TempC2):current_pos]) <= quantile(data_set_2$TempC[(current_pos - 241):(current_pos - countBW.TempC2)], probs = probS[3])) && 
            (min(data_set_2$TempC[(current_pos-countBW.TempC2):current_pos]) < bounds.TempC[1])) {
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if (POINTS) {
    ##store points - add show history flag
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$TempC[current_pos], DIRECTION, max(countAB.TempC2, countBW.TempC2))
    alarms.TempC2 <<- rbind(alarms.TempC2,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.TempC2.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


################################################################################
#       alerts.Redox
#
################################################################################
alerts.Redox <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95), 
                         period.to.show = 720, bounds.Redox = c(6.5, 7), wait = c(20, 20)) {
  
  trendline.Redox = runquantile(data_set_2$Redox, 241, probs = c(0.5,0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if(data_set_2$Redox[current_pos] >= trendline.Redox[, 1][current_pos]) {
    countAB.Redox <<- countAB.Redox + 1
    countBW.Redox <<- 0
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if ((countAB.Redox >= wait[1]) && 
          (mean(data_set_2$Redox[(current_pos-countAB.Redox):current_pos]) >= quantile(data_set_2$Redox[(current_pos - 241):(current_pos - countAB.Redox)], probs = probS[4])) &&
          (max(data_set_2$Redox[(current_pos-countAB.Redox):current_pos]) > bounds.Redox[2])) {
      POINTS <- TRUE
      DIRECTION <- "AB"
    } 
  }
  
  if(data_set_2$Redox[current_pos] < trendline.Redox[, 1][current_pos]) {
    countAB.Redox <<- 0
    countBW.Redox <<- countBW.Redox + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if ((countBW.Redox >= wait[2]) && 
          (mean(data_set_2$Redox[(current_pos-countBW.Redox):current_pos]) <= quantile(data_set_2$Redox[(current_pos - 241):(current_pos - countBW.Redox)], probs = probS[3])) &&
          (min(data_set_2$Redox[(current_pos-countBW.Redox):current_pos]) < bounds.Redox[1])){
      POINTS <- TRUE
      DIRECTION <- "BW"
    }
  }
  
  if (POINTS == TRUE) {
    ##store points - add show history flag
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$Redox[current_pos], DIRECTION, max(countAB.Redox, countBW.Redox))
    alarms.Redox <<- rbind(alarms.Redox,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.Redox.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}

################################################################################
#       alerts.DisOxy
#
################################################################################
alerts.DisOxy <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95),
                          period.to.show = 720, bounds.DisOxy = c(6.5, 7), wait = c(20, 20)) {
  
  trendline.DisOxy = runquantile(data_set_2$DisOxy, 241, probs=c(0.5, 0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if(data_set_2$DisOxy[current_pos] >= trendline.DisOxy[,1][current_pos]){
    countAB.DisOxy <<- countAB.DisOxy + 1
    countBW.DisOxy <<- 0
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if ((countAB.DisOxy >= wait[1]) &&
          (mean(data_set_2$DisOxy[(current_pos-countAB.DisOxy):current_pos]) >= quantile(data_set_2$DisOxy[(current_pos - 241):(current_pos - countAB.DisOxy)], probs = probS[4])) &&
          (max(data_set_2$DisOxy[(current_pos-countAB.DisOxy):current_pos]) > bounds.DisOxy[2])) {
      POINTS <- TRUE
      DIRECTION <- "AB"
    }
  }
  
  if(data_set_2$DisOxy[current_pos] < trendline.DisOxy[,1][current_pos]) {
    countAB.DisOxy <<- 0
    countBW.DisOxy <<- countBW.DisOxy + 1
    
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((countBW.DisOxy >= wait[2]) &&
         (mean(data_set_2$DisOxy[(current_pos-countBW.DisOxy):current_pos]) <= quantile(data_set_2$DisOxy[(current_pos - 241):(current_pos - countBW.DisOxy)], probs = probS[3])) &&
         (min(data_set_2$DisOxy[(current_pos-countBW.DisOxy):current_pos]) < bounds.DisOxy[1])) {
      POINTS <- TRUE
      DIRECTION <- "BW"
    }
  }
  
  if(POINTS) {
    ##store points - add show history flag
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$DisOxy[current_pos], DIRECTION, max(countAB.DisOxy, countBW.DisOxy))
    alarms.DisOxy <<- rbind(alarms.DisOxy, a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.DisOxy.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}

################################################################################
#       alerts.EC1
#
################################################################################
alerts.Cond1 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95), 
                       period.to.show = 720, bounds.EC = c(6.5, 7), wait = c(20, 20)) {
  
  trendline.EC=runquantile(data_set_2$Cond, 241, probs=c(0.5,0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if (data_set_2$Cond[current_pos] >= trendline.EC[, 1][current_pos]) {
    countAB.EC1 <<- countAB.EC1 + 1
    countBW.EC1 <<- 0
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$Cond[(current_pos-countAB.EC1):current_pos])))&&
         (!is.na(max(data_set_2$Cond[(current_pos-countAB.EC1):current_pos])))&&
         (!is.na(quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countAB.EC1)], probs = probS[4])))){
      if ((countAB.EC1 >= wait[1]) &&
            (mean(data_set_2$Cond[(current_pos-countAB.EC1):current_pos]) >= quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countAB.EC1)], probs = probS[4])) &&
            (max(data_set_2$Cond[(current_pos-countAB.EC1):current_pos]) > bounds.EC[2])) {
        POINTS <- TRUE
        DIRECTION <- "AB"
      }
    }
  }
  
  if(data_set_2$Cond[current_pos] < trendline.EC[, 1][current_pos]) {
    countAB.EC1 <<- 0
    countBW.EC1 <<- countBW.EC1 + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$Cond[(current_pos-countBW.EC1):current_pos])))&&
         (!is.na(max(data_set_2$Cond[(current_pos-countBW.EC1):current_pos])))&&
         (!is.na(quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countBW.EC1)], probs = probS[3])))){
      if((countBW.EC1 >= wait[2]) &&
           (mean(data_set_2$Cond[(current_pos-countBW.EC1):current_pos])<= quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countBW.EC1)], probs = probS[3])) &&
           (min(data_set_2$Cond[(current_pos-countBW.EC1):current_pos]) < bounds.EC[1])) {
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if(POINTS == TRUE) {
    ##store points - add show history flag
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$Cond[current_pos], DIRECTION, max(countAB.EC1, countBW.EC1))
    alarms.EC1 <<- rbind(alarms.EC1,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.EC1.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}



################################################################################
#       alerts.EC2
#
################################################################################
alerts.Cond2 <- function(data_set_2, current_pos, probS = c(.05, .95, .05, .95), 
                       period.to.show = 720, bounds.EC = c(6.5, 7), wait = c(20, 20)) {
  
  trendline.EC=runquantile(data_set_2$Cond, 241, probs=c(0.5,0.75))
  POINTS <- FALSE
  DIRECTION <- NULL
  
  if (data_set_2$Cond[current_pos] >= trendline.EC[, 1][current_pos]) {
    countAB.EC2 <<- countAB.EC2 + 1
    countBW.EC2 <<- 0
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$Cond[(current_pos-countAB.EC2):current_pos])))&&
         (!is.na(max(data_set_2$Cond[(current_pos-countAB.EC2):current_pos])))&&
         (!is.na(quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countAB.EC2)], probs = probS[4])))){
      if ((countAB.EC2 >= wait[1]) &&
            (mean(data_set_2$Cond[(current_pos-countAB.EC2):current_pos]) >= quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countAB.EC2)], probs = probS[4])) &&
            (max(data_set_2$Cond[(current_pos-countAB.EC2):current_pos]) > bounds.EC[2])) {
        POINTS <- TRUE
        DIRECTION <- "AB"
      }
    }
  }
  
  if(data_set_2$Cond[current_pos] < trendline.EC[, 1][current_pos]) {
    countAB.EC2 <<- 0
    countBW.EC2 <<- countBW.EC2 + 1
    ###if smaller sample mean is greater then previous 2 hours and point outside bounds then  ALARM
    if((!is.na(mean(data_set_2$Cond[(current_pos-countBW.EC2):current_pos])))&&
         (!is.na(max(data_set_2$Cond[(current_pos-countBW.EC2):current_pos])))&&
         (!is.na(quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countBW.EC2)], probs = probS[3])))){
      if((countBW.EC2 >= wait[2]) &&
           (mean(data_set_2$Cond[(current_pos-countBW.EC2):current_pos])<= quantile(data_set_2$Cond[(current_pos - 241):(current_pos - countBW.EC2)], probs = probS[3])) &&
           (min(data_set_2$Cond[(current_pos-countBW.EC2):current_pos]) < bounds.EC[1])) {
        POINTS <- TRUE
        DIRECTION <- "BW"
      }
    }
  }
  
  if(POINTS == TRUE) {
    ##store points - add show history flag
    a <- c(data_set_2$MINUTES[current_pos], data_set_2$Cond[current_pos], DIRECTION, max(countAB.EC2, countBW.EC2))
    alarms.EC2 <<- rbind(alarms.EC2,a)
    write.table(cbind(a[1], a[2], a[3], a[4]), "alarms.EC2.dat", row.names = FALSE, append = TRUE, col.names = FALSE)
  }
}


################################################################################
#       Prepare Data & To.Database
#
################################################################################

PrepareData <- function(data, type) {
  
  dateAndTime <- cbind(as.data.frame(unlist(strsplit(data[3], " "))[1]), as.data.frame(unlist(strsplit(data[3], " "))[2]))
  colnames(dateAndTime) <- c("Date", "Time")
  
  suppliedTimeMinutes <- HandleSuppliedTime3(dateAndTime, "%d/%m/%Y", hourstart = 1, minstart = 4)
  if (suppliedTimeMinutes == 0) {
	  return (as.numeric(0))
  }
  
  returnData <- data.frame(MINUTES = HandleSuppliedTime3(dateAndTime, "%d/%m/%Y", hourstart = 1, minstart = 4),
                           Date = unlist(strsplit(data[3], " "))[1], Time = unlist(strsplit(data[3], " "))[2], Value=strsplit(data[4], "\r\n")[1])
  colnames(returnData) <- c("MINUTES", "Date", "Time", "Value")
  returnData
}




ToDatabase <- function(rawData, type) {
  data <- PrepareData(rawData, type)
  if (data == 0) {
	  #print(T)
	  return
  }
  else {
    con <- dbConnect(MySQL(), user="RData", password="griffith", dbname=type)
  
    currentDate <- as.Date(unlist(strsplit(rawData[3], " "))[1], format="%d/%m/%Y")
    currentMonth <- as.POSIXlt(currentDate)$mon + 1
    currentYear <- as.POSIXlt(currentDate)$year + 1900
  
    if(is.na(match(currentMonth, month.in.database))) {
      tableName <- paste(currentYear, currentMonth, sep="_")
      dbWriteTable(con, tableName, data, row.names = F, append=T)
      month.in.database <<- currentMonth
    }
    else {
      tableName <- paste(currentYear, month.in.database, sep = "_")
      dbWriteTable(con, tableName, data, row.names = F, append = T)
    }
    dbDisconnect(con)
  } 
}

#compress realtime data into tgz format
ToTgz <- function(filename, files) {
  tar(filename,files,compression='gzip',tar="tar")
}
