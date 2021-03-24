source('config/init.R')
source('config/mySQLConfig.R')

nps.q4 <- mungeNPSQ4()
a_signup=prepQueryObjects(merged_Q1_all$northstar_id)
yearAgo <- as.Date('2017-12-05') - 365

q <-
  paste0("SELECT
         c.northstar_id,
         c.signup_id,
        c.signup_source,
         date(c.signup_created_at) AS signup_date
         FROM quasar.campaign_activity c
         WHERE c.northstar_id IN",a_signup,"
         AND c.signup_created_at >= '",yearAgo,"'"
  )

qres <- runQuery(q, 'mysql')

lastSignup <-
  qres %>%
  mutate(signup_date = as.Date(signup_date)) %>%
  group_by(northstar_id) %>%
  summarise(last_signup = max(signup_date))

avgSignup <-
  qres %>%
  mutate(signup_date = as.Date(signup_date)) %>%
  group_by(northstar_id) %>%
  summarise(avg_signup = mean(signup_date))

survey<-merged_Q1_all%>%
  select(northstar_id,survey, Niche_engaged, Niche_unengaged, nps_cat)

avgSignup_merge<-merge(x=survey, y=avgSignup, by ="northstar_id", all=TRUE)

typical<-avgSignup_merge%>%
  filter(survey=='Nonniche')

niche<-avgSignup_merge%>%
  filter(survey=='niche')

niche_engaged<-avgSignup_merge%>%
  filter(Niche_engaged=='1')

niche_unengaged<-avgSignup_merge%>%
  filter(Niche_unengaged=='1')


sms<-avgSignup_merge%>%
  filter(survey=='sms')

ggplot(avgSignup, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of Q1 Respondents')

ggplot(typical, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of Typical Respondents')

ggplot(niche, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of Niche Respondents')

ggplot(niche_unengaged, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of Niche Unengaged Respondents')

ggplot(niche_engaged, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of Niche Engaged Respondents')

ggplot(sms, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of SMS Respondents')
