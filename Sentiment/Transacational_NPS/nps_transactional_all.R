library(lubricate)

#Pull SMS, Email and Web NPS Typeforms
nps_trans <- "select *
              from survey.nps_typeforms"
nps_trans <- runQuery(nps_trans)

##########################
####Get NPS scores####
#########################

### PULLING OLD NPS SCORES UP TIL NOW####
nps_trans <- nps_trans%>%
  mutate(month=month(submit_date),
         year=year(submit_date))
#Exclude old web scores, look at only aug scores -  present
nps_scores <- nps_trans%>%
  filter(month>=8)%>%
  group_by(channel,month)%>%
  summarise(count=n(),nps=getNPS(nps,10))

############################
##Save scores to Postgres##
###########################
channel <- pgConnect()

dbWriteTable(channel, c("survey", "nps_transactional"), nps_scores, row.names=F, append=T)

grant <- "grant select on survey.nps_transactional to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)

###########################################################################################
#### AUTOMATED SCRIPT for 2019 - USE THIS GOING FORWARD TO ADD NEW SCORES TO POSTGRES######
###########################################################################################
#Filter for current month
nps_month <- nps_trans%>%
  filter(!duplicated(nsid) & month(submit_date)== month(Sys.Date()) & year(submit_date)==year(Sys.Date()))
#Calculate NPS scores for current month
nps_month <- nps_month%>%
  group_by(channel,month)%>%
  summarise(count=n(),nps=getNPS(nps,10))

#Add these month's scores to Postgres NPS table
dbWriteTable(channel, c("survey", "nps_transactional"), nps_month, row.names=F, append=T)

grant <- "grant select on survey.nps_transactional to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)

#Create transactional NPS table
# nps_data <-
#   tibble(
#     collected_at = as.Date(c('2018-08-31','2018-08-31','2018-08-31','2018-10-31','2018-10-31','2018-10-31',
#                              '2018-11-30','2018-11-30','2018-11-30','2018-12-18','2018-12-18','2018-12-18')),
#     channel = c('sms','email','web','sms','email','web','sms','email','web','sms','email','web'),
#     score = c(27,-6, 32, 10,11,37,15,4,27,9,26,29)
#   )

