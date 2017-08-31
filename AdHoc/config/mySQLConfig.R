library(RMySQL)

quasarConnect <- function(credentials) {
  require(stringr)

  login <- readChar(credentials, file.info(credentials)$size)
  user <- str_split(login, ':')[[1]][1]
  pw <- str_split(login, ':')[[1]][2]

  con <-
    dbConnect(
    MySQL(),
    user=user,
    host='quasar-slave-new.c9ajz690mens.us-east-1.rds.amazonaws.com',
    password=pw,
    dbname='quasar'
    )

  return(con)

}

con <- quasarConnect('~/Documents/quasar_login.txt')
