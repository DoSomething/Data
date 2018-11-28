#upload SMS responses from October
sms_nps_oct <- read.csv('~/Documents/NPS/SMS/SMS Q4 NPS/October 2018/SMS Q4 October pulled Nov 16.csv')%>%
  mutate(user_id=as.character(user_id))
#remove dups
sms_nps_aug_20<-sms_nps_aug_20%>%
  filter(!duplicated(id))

#Select SMS members who have messaged Gambit or clicked on broadcast in last 2 weeks and remove any who responded last month*/
sms_invites<- glue_sql("SELECT DISTINCT mel.northstar_id
                       FROM public.member_event_log mel
                       left join public.users u
                       on u.northstar_id=mel.northstar_id
                       WHERE (mel.action_type = 'messaged_gambit' or mel.action_type = 'bertly_link_click') and mel.channel = 'sms' and mel.timestamp >=  now() - interval '2 weeks'
                       and u.sms_status IN ('less','pending','active') and mel.northstar_id not in ({nsids*})",
                    nsids = sms_nps_oct$user_id,
                    .con = pg
)

sms_invites_nov <- runQuery(sms_invites)

sms_invites_nov <- sms_invites_nov%>%
  rename(id=northstar_id)

#Export invites pulled on 11/27/2018
write.csv(sms_invites_nov, file = 'Q4 SMS invites November 27.csv')


