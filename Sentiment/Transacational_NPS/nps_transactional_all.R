#Pull SMS, Email and Web NPS Typeforms
nps_sms <- "select *
        from survey.nps_sms"
nps_sms <- runQuery(nps_sms)
#rename nps and nsid
nps_sms <- nps_sms%>%
  rename(nps=starts_with("Considering"),
         nsid=user_id)

nps_email <- "select *
from survey.nps_email"
nps_email <- runQuery(nps_email)
#rename nps and nsid
nps_email <- nps_email%>%
  rename(nps=starts_with("Considering"),
         nsid=user_id)

nps_web <- "select *
from survey.nps_web"
nps_web <- runQuery(nps_web)
#rename nps and nsid
nps_web <-nps_web%>%
  rename(nps=starts_with("Considering"),
         nsid=northstar_id)%>%
  select(-campaign_id,-legacy_campaign_id,-origin)
nps_web$source <- 'web'

########################
#Join all channel data#
#######################
nps_trans <- bind_rows(nps_email,nps_sms,nps_web)

#add in NPS category, create channel var (each month, change to relevant month in source == statement)
nps_trans<- nps_trans%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(Submit.Date..UTC., 1, 10), origin='1970-01-01'),
         channel=
           case_when(source=='emailDec' ~ 'email',
                     source=='smsDec' ~ 'sms',
                     source=='web' ~ 'web'))

##########################
####Get SMS NPS scores####
#########################
nps_sms_month <- nps_trans%>%
  filter(channel=='sms' & !duplicated(nsid))
getNPS(nps_sms_month$nps,10)%>%
  print(nps_score)

#Get Email NPS score
nps_email_month <- nps_trans%>%
  filter(channel=='email' & !duplicated(nsid))
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

dbWriteTable(channel, c("survey", "nps_transactional"), nps_data, row.names=F)

grant <- "grant select on survey.nps_transactional to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)
