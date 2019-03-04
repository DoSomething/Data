#upload SMS responses from Typeform https://admin.typeform.com/form/OmFfrP/results
sms_nps_march <- read.csv('~/Documents/NPS/SMS/SMS Q1 2019 NPS/DoSomething SMS February 2019.csv')%>%
  mutate(user_id=as.character(user_id))
#remove dups
sms_nps_march<-sms_nps_march%>%
  filter(!duplicated(user_id))

#Select SMS members who have messaged Gambit or clicked on broadcast in last 2 weeks and remove any who have already responded 
sms_invites_march<- glue_sql("SELECT DISTINCT mel.northstar_id
                              FROM public.member_event_log mel
                              LEFT JOIN public.users u
                              ON u.northstar_id=mel.northstar_id
                              WHERE (mel.action_type = 'messaged_gambit' or mel.action_type = 'bertly_link_click') 
                                and mel.channel = 'sms' 
                                and mel.timestamp >=  now() - interval '2 weeks'
                                and u.sms_status IN ('less','pending','active') 
                                and mel.northstar_id not in ({nsids*})",
                              nsids = sms_nps_march$user_id,
                              .con = pg
)

sms_invites_march <- runQuery(sms_invites_march)

sms_invites_march <- sms_invites_march%>%
  rename(id=northstar_id)

#Export invites pulled on 03/04/2018
write.csv(sms_invites_march, file = 'Q1 SMS invites March 4th.csv')


