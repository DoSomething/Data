library(RMySQL)

pw_file <- '~/Documents/quasar_pw.txt'
pw <- readChar(pw_file, file.info(pw_file)$size)

con <-
  dbConnect(
  MySQL(),
  user='shasan',
  host='quasar-slave-new.c9ajz690mens.us-east-1.rds.amazonaws.com',
  password=pw,
  dbname='quasar'
  )

rm(pw_file, pw)