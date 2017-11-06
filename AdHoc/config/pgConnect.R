library(RPostgreSQL)

pgConnect <- function(db=c('torod','quasar','postgres')) {
  
  drv <- dbDriver("PostgreSQL")
  user=Sys.getenv('QUASAR_USERNAME')
  pw=Sys.getenv('QUASAR_PG_PW')
  
  channel <- 
    dbConnect(
      drv, 
      user = "shasan", 
      password=pw, 
      dbname = "torod", 
      host = "quasar-pg.c9ajz690mens.us-east-1.rds.amazonaws.com", 
      port = 5432
      )
  
  return(channel)
  
}

channel <- pgConnect(db='torod')