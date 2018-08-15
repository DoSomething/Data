library(dplyr)
source('config/init.R')
source('config/customFunctions.R')

#Pull Rogue Rbs for Poll Call 8/8 - 8/14/2018
pg <- pgConnect()

poll_call <-"select
u.first_name,
age(now(),u.birthdate) as age,
ca.caption
from campaign_activity ca
left join public.users u
on u.northstar_id = ca.northstar_id
where ca.campaign_id in ('8167') and ca.post_created_at >= '2018-08-08' and ca.post_created_at <'2018-08-15'"

poll_call <- runQuery(poll_call)

#format age so it's only years (first two characters)
poll_call <- poll_call %>%
  mutate(age=substr(poll_call$age, start =1 , stop =2))

write.csv(poll_call, file = 'Poll Call Rbs August 8-14th.csv')
