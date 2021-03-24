source('config/init.R')
source('config/mySQLConfig.R')

nps.q4 <- mungeNPSQ4()
a=prepQueryObjects(nps.q4$northstar_id)
yearAgo <- as.Date('2017-12-05') - 365

q <-
  paste0("SELECT
           c.northstar_id,
           c.signup_id,
           date(c.signup_created_at) AS signup_date
         FROM quasar.campaign_activity c
         WHERE c.northstar_id IN",a,"
         AND c.signup_created_at >= '",yearAgo,"'"
         )

qres <- runQuery(q, 'mysql')

avgSignup <-
  qres %>%
  mutate(signup_date = as.Date(signup_date)) %>%
  group_by(northstar_id) %>%
  summarise(avg_signup = mean(signup_date))

ggplot(avgSignup, aes(x=avg_signup)) +
  geom_density() +
  labs(x='Average Signup Date', title = 'Average Signup Date of Respondents')
