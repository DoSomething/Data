source('config/pgConnect.R')
channel <- pgConnect()

#Upload Typeform csvs to PostGres
nps_sms <- read.csv('~/Downloads/DoSomething SMS Q4 (1).csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         start_date =starts_with("Start"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID)

nps_email <- read.csv('~/Downloads/DoSomething Email Q4 (3).csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         start_date =starts_with("Start"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID)

nps_web <- read.csv('~/Downloads/Would you take this quick and easy survey_ (9).csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("Any"),
         start_date =starts_with("Start"),
         submit_date=starts_with("Submit"),
         nsid=northstar_id)%>%
  select(-X.,-Network.ID, -campaign_id, -legacy_campaign_id,-origin)
nps_web$source <- 'web'


########################
#Join all channel data#
#######################
nps_typeforms <- bind_rows(nps_email,nps_sms,nps_web)

#add in NPS category, create channel var (each month, change to relevant month in source == statement)
nps_typeforms<- nps_typeforms%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(submit_date, 1, 10), origin='1970-01-01'),
         channel=
           case_when(source %like% "email" ~ 'email',
                     source %like% "sms" ~ 'sms',
                     source=='web' ~ 'web'))

dbWriteTable(channel, c("survey", "nps_typeforms"), nps_typeforms, row.names=F)
grant <- "grant select on survey.nps_typeforms to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)
