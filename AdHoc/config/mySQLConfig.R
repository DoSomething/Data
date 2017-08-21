library(RMySQL)
library(stringr)

quasarConnect <- function(path) {
  pw_file <- path

  login <- readChar(pw_file, file.info(pw_file)$size)
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
