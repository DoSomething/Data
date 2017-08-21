source('config/init.R')
source('config/mySQLConfig.R')
library(tidyverse)
library(scales)

d5 <- runQuery('Scripts/five_days_five_actions_RBS.sql')

d5.n <-
  d5 %>%
  group_by(northstar_id) %>%
  summarise(
    reportbacks = sum(!is.na(post_id)),
    reportbacks.accepted = sum(!is.na(post_id) & status=='accepted'),
    reportbacks.SMS = sum(!is.na(post_id) & post_source=='sms-mobilecommons' & status=='accepted'),
    reportbacks.Web = sum(!is.na(post_id) & post_source=='phoenix-next' & status=='accepted')
  ) %>%
  mutate(
    Reportbacks = ifelse(reportbacks.accepted >= 3, '3+', reportbacks.accepted),
    Reportbacks.All = ifelse(reportbacks >= 3, '3+', reportbacks)
  ) 

Total <-
  d5.n %>%
  group_by(Reportbacks.All) %>%
  summarise(
    Count = n()
  ) %>%
  mutate(
    Pct.Breakdown = percent(Count / sum(Count))
  ) %>%
  arrange(Reportbacks.All) %>% print()

Type <- 
  d5.n %>%
  summarise(
    proportionSMS = percent(sum(reportbacks.SMS) / sum(reportbacks.accepted)),
    proportionWeb = percent(sum(reportbacks.Web) / sum(reportbacks.accepted))
  ) %>% print()
  

