library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con2 <- dbConnect(drv, host='192.168.30.11', port='5432', dbname='WQIAS', user='postgres', password='griffith24')
con <- dbConnect(drv, host='localhost', port='5432', dbname='WQIAS', user='postgres', password='griffith24')

## Submit and execute the query
#event1 <- dbGetQuery(con, "SELECT * FROM \"public\".\"Event1\"")

## fetch all elements from the result set
fetch(rs,n=-1)

### List tables in the database
dbListTables(con)

## Submit and execute the query
eventfin <<- dbGetQuery(con, "SELECT * FROM events.event_fingerprint_info")
event <<- dbGetQuery(con, "SELECT * FROM events.syd2event_14")

dataset.pH1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_ph1")
dataset.tempc1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_tempc1")
dataset.turbs1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_turbs1")
dataset.cond1 <<- dbGetQuery(con, "SELECT * FROM sydney2.sydney2_cond1")

event <<- dbGetQuery(con, "SELECT * FROM sydney2.sydn_ph1")


pH
dataset.pH
dataset.pH
dataset.pH


event <<- dbGetQuery(con, "SELECT * FROM events.")


impacts <- dbGetQuery(con2, "SELECT * FROM events.impacts")

summary(event)

for(i in 3:14){
  
  event[[i]] <- as.numeric(as.character(event[[i]]))
 
  
}



for(i in 9){
  
  event[[i]] <- as.numeric(as.character(event[[i]]))
  
  
}

colnames(event) <- colnames(event2) 

## split event 
a <- event[event$pH >= 9,]

1442, 3550, 6275
event1 <- event[1:1700,]
event2 <- event[1701:3700,]
event3 <- event[4201:(7100),]

event <- event3

#s <- sprintf("create table %s(%s, primary key(%s))", "DF",
#                             paste(names(DF), collapse = ", "),
#                             names(DF)[1])


#dbGetQuery(con, s)
#dbWriteTable(con, "DF", DF, append = TRUE, row.names = FALSE)

dbGetQuery(con, "ALTER TABLE events.impacts ADD PRIMARY KEY(event_id,sensor)")

#dbGetQuery(conn, "CREATE TABLE impacts (primary key event sensor)")

#dbWriteTable(conn, "Data", insertdata, append=TRUE)


dbWriteTable(con, c("events","event14a"), event, row.names=FALSE, append=FALSE, overwrite=TRUE)

dbWriteTable(con2, c("events","event_fingerprint_info"), eventfin, row.names=FALSE, append=FALSE, overwrite=TRUE)


event <- cbind(event.TempC[,1:3],event.DO[,3],event.Cond[,3],event.pH[,3],event.TurbS[1:3771,3],event.ORP[,3:4])


title.txt <- "Event 8"

dbRemoveTable(con2,  c("events","event_fingerprint_info"))




## Closes the connection
dbDisconnect(con)

## Frees all the resources on the driver
dbUnloadDriver(drv)
