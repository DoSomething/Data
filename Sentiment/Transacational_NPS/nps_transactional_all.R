#Pull SMS, Email and Web NPS Typeforms
nps_trans <- "select *
        from survey.nps_typeforms"
nps_trans <- runQuery(nps_trans)

##########################
####Get NPS scores####
#########################

#Each month change to relevant month in source== statement
nps_sms_month <- nps_trans%>%
  filter(channel=='sms' & !duplicated(nsid) & source =='smsDec')
getNPS(nps_sms_month$nps,10)%>%
  print(nps_score)

#Each month change to relevant month in source== statement
nps_email_month <- nps_trans%>%
  filter(channel=='email' & !duplicated(nsid) & source =='emailDec')
getNPS(nps_email_month$nps,10)%>%
  print(nps_score)

#Get Web NPS score, filter to submit date month range (change each month)
nps_web_month <- nps_trans%>%
  filter(channel=='web' & !duplicated(nsid) & submit_date>='2018-12-01' & submit_date < '2019-01-01')
getNPS(nps_web_month$nps,10)%>%
  print(nps_score)


############################
##Save scores to Postgres##
###########################

channel <- pgConnect()

#Create transactional NPS table
nps_data <-
  tibble(
    collected_at = as.Date(c('2018-08-30','2018-09-07','2018-09-30','2018-10-24','2018-10-24','2018-10-31',
                             '2018-11-28','2018-11-28','2018-11-28','2018-12-18','2018-12-18','2018-12-18')),
    channel = c('sms','email','web','sms','email','web','sms','email','web','sms','email','web'),
    score = c(27,-6, 32, 10,11,37,15,4,27,9,29,29)
  )

dbWriteTable(channel, c("survey", "nps_transactional"), nps_data, row.names=F, append=T)

grant <- "grant select on survey.nps_transactional to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)
