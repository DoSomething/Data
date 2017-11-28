source('config/init.R')
source('config/mySQLConfig.R')

#import Ride and Seek experiment group, remove 1 from beginning of phone number (c.io automatically saves numbers with 1 at start)
experiment<-read.csv('~/Documents/Ride and Seek/Ride and Seek 12_1 Experiment.csv')%>%
mutate (mobile = substr(mobile,2,11))

b<-prepQueryObjects(experiment$mobile)

#Query in db so it pulls NSIDs needed for uploading to c.io
q<- paste0("
             SELECT northstar_id
             FROM quasar.users
             WHERE mobile in ", b)

qres <- runQuery(q, which = 'mysql')

write.csv(qres, file ='Ride and Seek 12_1 Experiment group.csv')