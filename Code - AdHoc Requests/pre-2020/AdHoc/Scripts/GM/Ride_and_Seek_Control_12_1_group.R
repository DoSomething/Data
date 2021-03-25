source('config/init.R')
source('config/mySQLConfig.R')

#import Ride and Seek recipients csv Sohaib created and include only control groups that weren't messaged
#prepQueryObjects selects only the mobile column and concatenates all the mobile numbers into a string
control<-read.csv('~/Documents/Ride and Seek/Survey_Recipients (3).csv')%>%
  filter(group %in% c("10-19", "10-21", "12-11", "11-17") & (type %in% c("Control")))
a<-prepQueryObjects(control$mobile)

#Query in db so it pulls NSIDs needed for uploading to c.io
q<- paste0("
  SELECT northstar_id
FROM quasar.users
WHERE mobile in ", a)

qres <- runQuery(q, which = 'mysql')

write.csv(qres, file ='Ride and Seek 12_1 Control group.csv')



