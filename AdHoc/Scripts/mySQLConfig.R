library(RMySQL)

system('ssh -f shasan@jump.d12g.com')

dbConnect(
  MySQL(),
  user='shasan',
  host='quasar-slave-new.c9ajz690mens.us-east-1.rds.amazonaws.com',
  password='K8Ba9fkk00HnWOBtmVwNCG3r3JgKN7',
  dbname='quasar'
  )