
#upload invites from last quarter
sms_nps_q3 <- read.csv('~/Documents/NPS/SMS/SMS Q3 NPS/SMS Invites from Q3 (Aug-Sept).csv')%>%
  mutate(id=as.character(id))
#dedup
sms_nps_q3<-sms_nps_q3%>%
  filter(!duplicated(id))

#Select SMS members who have messaged Gambit or clicked on broadcast in last 2 weeks
#exclude those who were surveyed last month*/
sms_invites<- glue_sql("SELECT DISTINCT mel.northstar_id as id
                       FROM public.member_event_log mel
                       inner join public.users u
                       on u.northstar_id=mel.northstar_id
                       WHERE (mel.action_type = 'messaged_gambit' or mel.action_type = 'bertly_link_click') 
                       and mel.channel = 'sms' 
                       and mel.action_type = 'bertly_link_click' >= now() - interval '14 DAYS'
                       and u.sms_status IN ('less','pending','active') and mel.northstar_id not in ({nsids*})
                       group by 1",
                    nsids = sms_nps_q3$id,
                    .con = pg
)

sms_invites <- runQuery(sms_invites)

#export ids for Oct
write.csv(sms_invites, file = 'Q4 SMS invites October.csv')

