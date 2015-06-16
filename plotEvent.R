



win.graph()
par(mfcol=c(4,1))
par(mar=c(0,4,1,2))


#event <- syd2event9

plot(event$MINUTES,event$pH, type="l", axes=FALSE, ylab="pH",xlab="")
- 
lines(runquantile(event$pH, 241, probs=c(0.5)))
box()
axis(2)
#identify(event$pH,event$MINUTES )

par(mar=c(0,4,1,2))
plot(event$MINUTES,event$TempC,  type="l",axes=FALSE, ylab="Temp",xlab="")
box()
axis(2)

par(mar=c(0,4,1,2))
plot(event$MINUTES,event$Cond,  type="l", axes=FALSE, ylab="Cond",xlab="")
axis(2)
box()

par(mar=c(5,4,1,2))
plot(event$MINUTES,event$TurbS,  type="l",axes=FALSE, ylab="Turb",xlab="")
axis(2)
box()


title(sub = "syd2event_23",col="red", cex.sub = 3.75, font.sub = 3, col.sub = "red")

#LabelTimeAxis()
#identify(event$MINUTES,event$pH, labels=name, plot=TRUE)

#------------------------------------------------------------------------
#
#         PLOT REAL TIME TEST EVENT
#
#------------------------------------------------------------------------

event <- syd2event_1
event <<- dbGetQuery(con, "SELECT * FROM events.syd2event_12")


win.graph()
par(mfcol=c(4,1))
par(mar=c(0,4,4,2))


plot(dataset$pH$MINUTES, dataset$pH$pH, type="l", axes=FALSE, ylab="pH",xlab="")
points(alarms.pH1$MINUTES[alarms.pH1$MINUTES > 7844660],alarms.pH1$pH[alarms.pH1$MINUTES > 7844660], col="green")
#lines(dataset$pH$MINUTES, runmed(dataset$pH$pH, 701), col = "blue")
box()
axis(2)

par(mar=c(0,4,1,2))
plot(dataset$TempC$MINUTES[!is.na(dataset$TempC$TempC)],dataset$TempC$TempC[!is.na(dataset$TempC$TempC)],  type="l",axes=FALSE, ylab="Temp",xlab="")
points(alarms.TempC1$MINUTES[alarms.TempC1$MINUTES > 7844600],alarms.TempC1$TempC[alarms.TempC1$MINUTES > 7844600], col="green")
#lines(dataset$TempC$MINUTES, runmed(dataset$TempC$TempC[!is.na(dataset$TempC$TempC)], 701), col = "blue")
box()
axis(2)


par(mar=c(0,4,1,2))
plot(dataset$Cond$MINUTES,dataset$Cond$Cond,  type="l", axes=FALSE, ylab="Cond",xlab="")
points(alarms.Cond1$MINUTES[alarms.Cond1$MINUTES > 7844660],alarms.Cond1$Cond[alarms.Cond1$MINUTES > 7844660], col="green")
#lines(dataset$Cond$MINUTES, runmed(dataset$Cond$Cond, 701), col = "blue")
axis(2)
box()

par(mar=c(5,4,1,2))
plot(dataset$TurbS$MINUTES,dataset$TurbS$TurbS,  type="l",axes=FALSE, ylab="Turb",xlab="")
points(alarms.TurbS1$MINUTES[alarms.TurbS1$MINUTES > 7844660],alarms.TurbS1$TurbS[alarms.TurbS1$MINUTES > 7844660], col="green")
#lines(dataset$TurbS$MINUTES, runmed(dataset$TurbS$TurbS, 701), col = "blue")
axis(2)
box()

#LabelTimeAxis()
#par(mar=c(5,4,1,2))
#plot(event1$MINUTES,event1$ORP,  type="l",axes=FALSE, ylab="ORP",xlab="")
#axis(2)
#box()



title(sub = title.txt,col="red", cex.sub = 1.75, font.sub = 3, col.sub = "red")
LabelTimeAxis()


#------------------------------------------------------------------------
#
#         PLOT SUBSET CURVE EXTRACTION
#
#------------------------------------------------------------------------

## Sydney2 event 12

event.start <- 7844633
event.end <- 7844803


## event 8  - short version
event.start <- 4710139
event.end <- 4710200




# event 9
#"4795583" "0" "NONE"
#"4795737" "8" "SINGLE-PH"
#"4795752" "12" "NONE"
#"4795787" "8" "SINGLE-PH"
#"4795952" "0" "NONE"

event.start <- 4795737
event.end <- 4795952

# event 6
#"4685628" "0" "NONE"
#"4686818" "8" "SINGLE-PH"
#"4686836" "0" "NONE"
#"4686853" "8" "NONE"
#"4686914" "0" "NONE"

event.start <- 4686853
event.end <- 4686914


# event 10
"4685624" "8" "SINGLE-PH"
"4685628" "0" "NONE"
"6136599" "2" "NONE"
"6136653" "0" "NONE"
"6137603" "2" "NONE"
"6137615" "0" "NONE"
"6138026" "8" "SINGLE-PH"
"6138048" "0" "NONE"
"6138139" "2" "NONE"
"6138151" "0" "NONE"
"6138179" "2" "NONE"
"6138183" "0" "NONE"
"6138224" "8" "NONE"
"6138225" "10" "TURB-IGNORE"
"6138257" "0" "NONE"
event.start <-6138224 # 6138026  #vs 6138179 vs 6138224
event.end <- 6138257

#event 4
"4602652" "0" "NONE"
"4603440" "8" "SINGLE-PH"
"4603446" "0" "NONE"
"4604348" "2" "NONE"
"4604356" "0" "NONE"
"4604374" "2" "NONE"
"4604398" "0" "NONE"
"4604867" "8" "NONE"
"4604878" "0" "NONE"
event.start <-4604867 
event.end <- 4604878



# event 5
"4622844" "0" "NONE"
"4622863" "4" "NONE"
"4622866" "0" "NONE"
"4623567" "2" "NONE"
"4623579" "10" "TURB-IGNORE"
"4623600" "8" "SINGLE-PH"
"4623627" "9" "NONE"
"4623633" "11" "NONE"
"4623652" "3" "TURB-IGNORE"
"4623658" "1" "NONE"
"4623689" "0" "NONE"
"4623901" "8" "SINGLE-PH"
"4623915" "0" "NONE"


event.start <-4623567 # 6138026  #vs 6138179 vs 6138224
event.end <- 4623689

# -- event 11
"4689022" "0" "NONE"
"4689954" "8" "SINGLE-PH"
"4689999" "0" "NONE"

event.start <-4689954 
event.end <- 4689999

# -- event 9
event.start <-4795718 
event.end <- 4795952

# event 2
"4492270" "8" "SINGLE-PH"
"4492273" "0" "NONE"

# event 2a
"4493599" "0" "NONE"
"4493978" "8" "SINGLE-PH"
"4494001" "0" "NONE"

#event 7
"4699057" "0" "NONE"
"4698935" "2" "NONE"
"4698948" "6" "TURB-IGNORE"
"4698983" "14" "NONE"
"4699011" "15" "NONE"
"4699044" "14" "NONE"
"4699050" "10" "TURB-IGNORE"
"4699055" "8" "SINGLE-PH"
"4699234" "0" "NONE"

# event 14 & 14a
"4793230" "0" "NONE"
"4793532" "8" "NONE"
"4793553" "0" "NONE"
"4793772" "8" "SINGLE-PH"
"4793791" "0" "NONE"

event.start <-4793772
event.end <- 4793791

#impacts <- dbGetQuery(con, "SELECT * FROM events.impacts" )
#impacts[impacts$event_id == "event10",]

#event.start <- dbGetQuery(con, "SELECT startminute FROM events.impacts WHERE event_id = event10" )
#event.end <- dbGetQuery(con, "SELECT * FROM events.impacts WHERE event_id <> event10 " )


event <<- dbGetQuery(con, "SELECT * FROM events.event10")

event <- event[event$MINUTES>(event.start-30),]
event <- event[event$MINUTES<=event.end+3,]

win.graph()
par(mfcol=c(4,1))
par(mar=c(0,4,1,2))


plot(event$MINUTES,event$pH, type="l", axes=FALSE, ylab="pH",xlab="")
lines(alarms)
box()
axis(2)

par(mar=c(0,4,1,2))
plot(event$MINUTES,event$Temp,  type="l",axes=FALSE, ylab="Temp",xlab="")
box()
axis(2)


par(mar=c(0,4,1,2))
plot(event$MINUTES,event$Cond,  type="l", axes=FALSE, ylab="Cond",xlab="")
axis(2)
box()

par(mar=c(5,4,1,2))
plot(event$MINUTES,event$Turb,  type="l",axes=FALSE, ylab="Turb",xlab="")
axis(2)
box()


title(sub = title.txt,col="red", cex.sub = 1.75, font.sub = 3, col.sub = "red")
LabelTimeAxis()



