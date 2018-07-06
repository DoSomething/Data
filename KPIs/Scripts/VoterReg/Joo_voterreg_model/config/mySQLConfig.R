library(RMySQL)

quasarConnect <- function() {
  require(RMySQL)
  user=Sys.getenv('QUASAR_USERNAME')
  pw=Sys.getenv('QUASAR_PASSWORD')

  con <-
    dbConnect(
    MySQL(),
    user=user,
    host='quasar-secondary.c9ajz690mens.us-east-1.rds.amazonaws.com',
    password=pw,
    dbname='quasar'
    )

  return(con)

}
