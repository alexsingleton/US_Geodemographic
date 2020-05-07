library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
pw<-"tower2020"
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "usgeodem",
                 host = "138.253.73.4", port = 5432,
                 user = "postgres", password = pw)
query<-"select * from data;" 
data<-dbGetQuery(con,query)


