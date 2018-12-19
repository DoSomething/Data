library(lubridate)
library(dplyr)
library(readr)
library(RPostgreSQL)

###########################################################################################
#### AUTOMATED SCRIPT for 2019 - USE THIS GOING FORWARD TO ADD NEW SCORES TO POSTGRES######
###########################################################################################
nps_sms <- read_csv('~/Downloads/DoSomething SMS.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID, -starts_with("Start"))

nps_email <- read_csv('~/Downloads/DoSomething Email.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID, -starts_with("Start"))

nps_web <- 
  read_csv('~/Downloads/DoSomething Web.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("Any"),
         submit_date=starts_with("Submit"),
         nsid=northstar_id)%>%
  select(-X.,-Network.ID, -campaign_id, -legacy_campaign_id,-origin, -starts_with("Start")) %>%
  mutate(
    source = 'web'
  )

#join typeforms and filter to only new responses for current month
today <- Sys.Date()

nps_typeforms <- 
  bind_rows(nps_email,nps_sms,nps_web) %>%
  filter(submit_date > today %m+% months(-1)) %>%
  mutate(nps_cat =
           case_when(nps<7 ~ 'Detractor',
                     nps %in% c(7,8) ~ 'Persuadable',
                     nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(submit_date, 1, 10), origin='1970-01-01'),
         channel=
           case_when(grepl('email',source) ~ 'email',
                     grepl('sms',source) ~ 'sms',
                     grepl('web',source) ~ 'web')
         )

#Add these responses to Postgres NPS table
dbWriteTable(channel, c("survey", "nps_typeforms"), nps_typeforms, row.names=F, append=T)

#Pull Typeform data from PostGres
nps_trans <- "select *
from survey.nps_typeforms"
nps_trans <- runQuery(nps_trans)

#Filter to current month
nps_month <- nps_trans%>%
  filter(
    !duplicated(nsid) & 
      month(submit_date)== month(Sys.Date()) & 
      year(submit_date)==year(Sys.Date())
    )

#Calculate NPS scores for current month
nps_month <- nps_month%>%
  group_by(channel,month)%>%
  summarise(count=n(),nps=getNPS(nps,10))

#Add these month's scores to Postgres NPS table
dbWriteTable(channel, c("survey", "nps_transactional"), nps_month, row.names=F, append=T)



#Upload Typeform csvs to PostGres
# 
# forms <- c('SMS','Email','Web')
# 
# nps <-
#   read_csv(paste0('~/Downloads/DoSomething ', forms[1],'.csv')) %>%
#   select(
#     nps=starts_with('Considering'), 
#     submit_date=starts_with("Submit"), 
#     nsid=user_id
#   ) %>%
#   bind_rows(
#     read_csv(paste0('~/Downloads/DoSomething ', forms[2],'.csv') %>%
#                select(
#                  nps=starts_with('Considering'), 
#                  submit_date=starts_with("Submit"), 
#                  nsid=user_id
#                )
#              ),
#     read_csv(paste0('~/Downloads/DoSomething ', forms[3],'.csv')) %>%
#       select(
#         user_id = northstar_id,
#         nps=starts_with('Considering'),
#         submit_date=starts_with("Submit")
#       )
#   )
