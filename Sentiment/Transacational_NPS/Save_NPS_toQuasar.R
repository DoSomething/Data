source('config/pgConnect.R')
channel <- pgConnect()

#Create transactional NPS table
nps_data <- 
  tibble(
    collected_at = as.Date(c('2018-08-30','2018-09-07','2018-09-30','2018-10-24','2018-10-24','2018-10-31')),
    channel = c('sms','email','web','sms','email','web'),
    score = c(27,-6, 32, 10,11,37)
  )

dbWriteTable(channel, c("survey", "nps_transactional"), nps_data, row.names=F)

grant <- "grant select on survey.nps_transactional to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)
