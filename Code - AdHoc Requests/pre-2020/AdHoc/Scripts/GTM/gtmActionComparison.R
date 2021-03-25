# https://trello.com/c/LgSDsmM7/1288-data-request-grab-the-mic-member-engagement

source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

qres <- runQuery('Scripts/gtm_action_comparison.sql')

# Voter Reg ---------------------------------------------------------------

voterRegGTM <- 
  qres %>% 
  filter(!duplicated(northstar_id) & !is.na(voter_registration_status)) %>% 
  count(did_gtm, voter_registration_status) %>% 
  group_by(did_gtm) %>% 
  mutate(p=n/sum(n))

saveCSV(voterRegGTM)

voterRegGTM.smsActive <- 
  qres %>% 
  filter(!duplicated(northstar_id) & !is.na(voter_registration_status)) %>% 
  count(did_gtm, sms_active, voter_registration_status) %>% 
  group_by(did_gtm, sms_active) %>% 
  mutate(p=n/sum(n))

saveCSV(voterRegGTM.smsActive)

voterRegGTM.smsOnly <- 
  qres %>% 
  filter(!duplicated(northstar_id) & !is.na(voter_registration_status)) %>% 
  count(did_gtm, sms_only, voter_registration_status) %>% 
  group_by(did_gtm, sms_only) %>% 
  mutate(p=n/sum(n))

saveCSV(voterRegGTM.smsOnly)

# Posts -------------------------------------------------------------------

set <- 
  qres %>% 
  filter(
    !is.na(action_month) & 
      action_type %in% c('post','facebook share completed')
      ) %>% 
  group_by(northstar_id, action_month) %>% 
  summarise(
    did_gtm = max(did_gtm),
    gtm_signup_month = max(gtm_signup_month),
    earliest_non_gtm_signup_month = max(earliest_non_gtm_signup_month),
    action_count = sum(action_count)
  )

munge <- 
  set %>% 
  mutate(
    gtm_actions = 
      case_when(
        did_gtm == 1 & action_month >= gtm_signup_month ~ action_count,
        TRUE ~ NA_real_
      ),
    non_gtm_actions =
      case_when(
        did_gtm == 0 | action_month < gtm_signup_month ~ action_count,
        TRUE ~ NA_real_
      )
  )

library(reshape2)

agg <- 
  munge %>% 
  filter(action_month<6) %>% 
  group_by(action_month) %>% 
  summarise(
    GtM = mean(gtm_actions, na.rm=T),
    NonGtM = mean(non_gtm_actions, na.rm=T)
  ) %>% 
  melt(
    variable.name='Group',
    id.var='action_month', 
    value.name = 'Reportbacks'
    )

ggplot(agg, aes(x=action_month, y=Reportbacks, colour=Group)) +
  geom_point() + geom_smooth() + ylim(1,2) +
  labs(
    title='Avg # of Reportbacks Per Month',
    x='Month'
  ) +
  theme(plot.title = element_text(hjust=.5))


# all actions -------------------------------------------------------------

col <- 
  qres %>% 
  filter(
    action_type %in% 
      c('facebook share completed','messaged_gambit','phoenix_page_action',
        'post','signup','site_access','site_login','clicked_link','email_interaction')
  ) %>% 
  mutate(
    action = 
      case_when(
        action_type %in% c('signup','facebook share completed','post','registered') ~ 'Campaigns',
        action_type %in% c('messaged_gambit','sms_link_click') ~ 'SMS',
        action_type %in% c('site_access','site_login','phoenix_page_action') ~ 'Web',
        action_type %in% c('clicked_link','email_interaction') ~ 'Email',
        TRUE ~ ''
      ),
    Group = ifelse(did_gtm==1, 'GtM','NonGtM')
    ) %>% 
  group_by(Group, action_month, action) %>%
  summarise(
    actions = mean(action_count)
  )

ggplot(col, aes(x=action_month, y=actions, color=Group)) +
  geom_point() + geom_smooth() +
  facet_wrap(~action, scale='free_y') +
  labs(
    title = 'Comparison Across All Touchpoints',
    x='Month',
    y='# of Interactions'
  ) + 
  theme(
    plot.title=element_text(hjust=.5),
    legend.position = 'bottom'
    ) 
