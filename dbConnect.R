library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host='localhost', port='5432', dbname='WQIAS',
                 user='postgres', password='griffith24')


## Submit and execute the query
#event1 <- dbGetQuery(con, "SELECT * FROM \"public\".\"Event1\"")

## fetch all elements from the result set
#fetch(rs,n=-1)

### List tables in the database
dbListTables(con)

## Submit and execute the query
event2 <- dbGetQuery(con, "SELECT * FROM events.event2")

title <- "Event 1"

## Closes the connection
dbDisconnect(con)

## Frees all the resources on the driver
dbUnloadDriver(drv)
