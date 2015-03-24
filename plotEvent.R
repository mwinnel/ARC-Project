win.graph()
par(mfcol=c(6,1))
par(mar=c(0,4,4,2))


plot(event1$MINUTES,event1$pH, type="l", axes=FALSE, ylab="pH",xlab="")
box()
axis(2)

par(mar=c(0,4,1,2))
plot(event1$MINUTES,event1$Temp,  type="l",axes=FALSE, ylab="Temp",xlab="")
box()
axis(2)

#LabelTimeAxis()
par(mar=c(0,4,1,2))
plot(event1$MINUTES,event1$DO,  type="l",axes=FALSE, ylab="DO",xlab="")
axis(2)
box()

par(mar=c(0,4,1,2))
plot(event1$MINUTES,event1$ORP,  type="l",axes=FALSE, ylab="ORP",xlab="")
axis(2)
box()

par(mar=c(0,4,1,2))
plot(event1$MINUTES,event1$Turb,  type="l",axes=FALSE, ylab="Turb",xlab="")
axis(2)
box()

#LabelTimeAxis()
par(mar=c(5,4,1,2))
plot(event1$MINUTES,event1$Cond,  type="l", axes=FALSE, ylab="Cond",xlab="")
axis(2)
box()

LabelTimeAxis()
