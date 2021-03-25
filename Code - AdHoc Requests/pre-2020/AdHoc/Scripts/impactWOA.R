source('Scripts/init.R')
source('Scripts/DSUtils.R')
library(scales)

WOACampaigns <- c(7458, 7542, 7597, 7633, 7546)

camp <- read_csv('Data/user_campaigns.csv') %>% tbl_dt() %>%
  mutate(user_campaign = paste0(drupal_uid, campaign_run_id)) %>%
  filter(!duplicated(user_campaign)) %>%
  select("drupal_uid","campaign_run_id","signup_created_at") %>%
  mutate(
    WOA = ifelse(campaign_run_id %in% WOACampaigns, 1, 0)
  ) 

camp[
  ,
  ':='(
    userWOA = max(WOA)
  )
  ,
  by = drupal_uid
][
  userWOA == 1
  ,
  ':='(
    signupWOAts = max(signup_created_at)
  )
  ,
  by = drupal_uid
] %>%
  filter(!duplicated(drupal_uid)) %>%
  select(drupal_uid, userWOA, signupWOAts) -> camp
  
events <- read_csv('Data/the_all_inclusive member_event_log 2017-07-26T1746.csv') %>% tbl_dt() %>%
  setNames(c('X1','drupal_uid','event_ts','event_id','action_id')) %>%
  filter(!is.na(drupal_uid)) %>%
  left_join(camp) %>%
  select(-X1) %>%
  mutate(
    userWOA = ifelse(is.na(userWOA), 0, userWOA),
    actionDiff = as.Date(signupWOAts) - as.Date(event_ts),
    Month = month(event_ts)
  )

actionDiff.WOA <-
  events %>%
  filter(userWOA == 1) %>%
  group_by(drupal_uid, actionDiff) %>%
  summarise(
    actionCount = n()
  ) %>%
  mutate(
    PrePost = ifelse(actionDiff < 0, 'Pre', 'Post')
  )

actionsOverTime.NonWOA <-
  events %>%
  filter(userWOA == 0) %>%
  group_by(drupal_uid, event_ts) %>%
  summarise(
    actionCount = n()
  )

WOA <-
  ggplot(actionDiff.WOA[actionDiff >= -100 & actionDiff < 175], aes(x=actionDiff, y=actionCount)) + 
  geom_smooth(aes(color=PrePost, group=PrePost)) + geom_smooth(aes(color=PrePost, group=PrePost), method='lm', linetype='dotdash' , size=.5, se=F) +
  geom_smooth(color='black', method='lm', linetype='dotdash' , size=.5, se=F) +
  scale_x_continuous(breaks=pretty_breaks(8)) + theme(legend.position="none") +
  labs(x='Days Elapsed Since Week of Action', y='Count of Actions', title='Actions Taken Per User since Week of Action')

noWOA <-
  ggplot(actionsOverTime.NonWOA, aes(x=event_ts, y=actionCount)) + 
  geom_smooth() + geom_smooth(method='lm', color='black', linetype='dotdash' ,size=.5, se=F) +
  labs(x='Time', y='Count of Actions', title='Actions Taken Per User Over Time - Excluding Week of Action') +
  scale_x_date(breaks=pretty_breaks(8)) #+ ylim(0,5)

finalPlot <- grid.arrange(WOA, noWOA, nrow = 2)

####

memberMonth <- 
  events %>%
  group_by(drupal_uid, Month) %>%
  summarise(
    Count = n()
  ) %>%
  right_join(
    expand.grid(drupal_uid = unique(events$drupal_uid), Month = unique(events$Month)) %>% tbl_dt(), copy=T 
  ) %>%
  mutate(
    potentially_active = 1,
    active = ifelse(!is.na(Count), 1, 0)
  ) %>%
  arrange(drupal_uid, Month) %>%
  filter(drupal_uid %in% actionDiff.WOA$drupal_uid) %>%
  group_by(Month) %>%
  summarise(
    proportionActive = sum(active) / sum(potentially_active)
  ) %>%
  mutate(Month = Month - 1, proportionActive = percent(proportionActive))
