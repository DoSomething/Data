#Script to consolidate all transactional NPS Typeforms into PostGres table
source('config/pgConnect.R')
channel <- pgConnect()

#Upload Typeform csvs to PostGres
nps_sms_q4 <- read.csv('~/Documents/NPS/Typeforms/DoSomething SMS Q4.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID, -starts_with("Start"))

nps_sms_q3 <- read.csv('~/Documents/NPS/Typeforms/SMS Q3.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID,-utm_source,-starts_with("Start"),-source)
nps_sms_q3$source <- 'smsSep'

nps_email_q4 <- read.csv('~/Documents/NPS/Typeforms/DoSomething Email Q4.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("You"),
         submit_date=starts_with("Submit"),
         nsid=user_id)%>%
  select(-X.,-Network.ID,-starts_with("Start"))

nps_email_q3 <- read.csv('~/Documents/NPS/Email/Q3/Email NPS Q3 September pulled nov 16.csv')%>%
  select(External.Reference,starts_with("How"), starts_with("You"),Timestamp..MM.dd.yyyy.)%>%
  rename(nsid=External.Reference,
         nps=starts_with("How"),
         nps_reason=starts_with("You"),
         submit_date=starts_with("Timestamp"))
nps_email_q3$source <- 'emailSep'

nps_web <- read.csv('~/Documents/NPS/Typeforms/Web Q4.csv')%>%
  rename(nps=starts_with("Considering"),
         nps_reason=starts_with("Any"),
         submit_date=starts_with("Submit"),
         nsid=northstar_id)%>%
  select(-X.,-Network.ID, -campaign_id, -legacy_campaign_id,-origin, -starts_with("Start"))
nps_web$source <- 'web'


########################
#Join all channel data#
#######################
nps_typeforms <- bind_rows(nps_email_q4,nps_email_q3,nps_sms_q3, nps_sms_q4,nps_web)

#add in NPS category, create channel var
nps_typeforms<- nps_typeforms%>%
  mutate(nps_cat =
           case_when(nps<7 ~ 'Detractor',
                     nps %in% c(7,8) ~ 'Persuadable',
                     nps>8 ~ 'Promoter'),
         submit_date=
           case_when(source=='emailSep'~ as.Date(paste0(submit_date, ':00'), '%m/%d/%y %H:%M:%S'),
                    !source=='emailSep' ~ as.Date(substr(submit_date, 1, 10), origin='1970-01-01')),
         channel=
            case_when(grepl('email',source) ~ 'email',
                      grepl('sms',source) ~ 'sms',
                      grepl('web',source) ~ 'web'))

dbWriteTable(channel, c("survey", "nps_typeforms"), nps_typeforms, row.names=F)
grant <- "grant select on survey.nps_typeforms to jli,shasan, mjain, ubrtjh45jniptr;"
dbGetQuery(channel, grant)
