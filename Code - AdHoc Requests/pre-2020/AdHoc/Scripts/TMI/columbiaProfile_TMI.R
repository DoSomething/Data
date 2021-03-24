source('config/init.R')
source('config/mySQLConfig.R')
library(scales)
library(openxlsx)

# qres <- runQuery('~/Desktop/hawaiiProfile_TMI.sql', which='mysql')

q<- paste0("
           SELECT
           users.*,
           c.signup_id,
           max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) reportback,
           i.campaign_node_id_title AS campaign,
           date(c.signup_created_at) as signup_date,
           i.campaign_type,
           i.campaign_action_type AS action_type,
           i.campaign_cause_type AS cause_type
           FROM
           (SELECT DISTINCT
           u.northstar_id,
           u.source,
           FLOOR(DATEDIFF(CURRENT_DATE(), u.birthdate) / 365) age,
           DATEDIFF(date(now()), date(u.created_at)) as 'days_a_member',
           COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state,
           u.country
           FROM quasar.users u
           LEFT JOIN quasar.moco_profile_import m
           ON (m.moco_id = u.moco_commons_profile_id AND u.moco_commons_profile_id IS NOT NULL)
           WHERE (u.customer_io_subscription_status = 'subscribed'
           OR u.sms_status = 'active')
           ) users
           LEFT JOIN quasar.campaign_activity c ON users.northstar_id=c.northstar_id
           LEFT JOIN quasar.campaign_info i ON i.campaign_run_id = c.campaign_run_id
           WHERE users.country = 'CO' OR users.country LIKE '%columbia%' OR users.country = '{value: CO}'
           GROUP BY c.signup_id
           ;")

qres <- runQuery(q, which = 'mysql')

user <-
  qres %>%
  mutate(
    source = case_when(
      grepl('niche', source) ~ 'niche',
      grepl('sms', source) ~ 'sms',
      TRUE ~ 'web'
    )
  ) %>%
  filter(!duplicated(northstar_id)) %>%
  summarise(
    activeMembers = n(),
    age = mean(age, na.rm=T),
    daysMember = mean(days_a_member, na.rm=T),
    propNiche = length(which(source=='niche')) / n(),
    propSMS = length(which(source=='sms')) / n(),
    propWeb = length(which(source=='web')) / n()
  ) %>%
  bind_cols(
    data.frame(
      signups = nrow(qres),
      reportbacks = sum(qres$reportback))
  )

campaign_type <-
  qres %>% count(campaign_type) %>% mutate(proportion = n/sum(n))
action_type <-
  qres %>% count(action_type) %>% arrange(-n) %>% mutate(proportion = n/sum(n))
cause_type <-
  qres %>% count(cause_type) %>% arrange(-n) %>% mutate(proportion = n/sum(n))
campaign <-
  qres %>% filter(as.Date(signup_date)>='2016-01-01') %>%
  count(campaign) %>% arrange(-n) %>% mutate(proportion = n/sum(n))

wb <- createWorkbook()

addWorksheet(wb, 'User')
addWorksheet(wb, 'campaign_type')
addWorksheet(wb, 'action_type')
addWorksheet(wb, 'cause_type')
addWorksheet(wb, 'campaign')
writeData(wb, 'User', user, rowNames=F)
writeData(wb, 'campaign_type', campaign_type, rowNames=F)
writeData(wb, 'action_type', action_type, rowNames=F)
writeData(wb, 'cause_type', cause_type, rowNames=F)
writeData(wb, 'campaign', campaign, rowNames=F)

saveWorkbook(
  wb,
  paste0('~/Desktop/columbiaMemberDescriptiveStats.xlsx'),
  overwrite = TRUE
)