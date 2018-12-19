library(lubridate)
library(dplyr)
library(RPostgreSQL)

###########################################################################################
#### AUTOMATED SCRIPT for 2019 - USE THIS GOING FORWARD TO ADD NEW SCORES TO POSTGRES######
###########################################################################################

#Pull Typeform data from PostGres
nps_trans <- "select *
from survey.nps_typeforms"
nps_trans <- runQuery(nps_trans)

#Filter to current month
nps_month <- nps_trans%>%
  filter(!duplicated(nsid) & month(submit_date)== month(Sys.Date()) & year(submit_date)==year(Sys.Date()))

#Calculate NPS scores for current month
nps_month <- nps_month%>%
  group_by(channel,month)%>%
  summarise(count=n(),nps=getNPS(nps,10))

#Add these month's scores to Postgres NPS table
dbWriteTable(channel, c("survey", "nps_transactional"), nps_month, row.names=F, append=T)
