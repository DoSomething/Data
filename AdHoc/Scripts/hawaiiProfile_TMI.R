#https://trello.com/c/hhvPK9Rq/1185-hawaii-data
source('config/init.R')
source('config/mySQLConfig.R')
library(scales)
library(openxlsx)

qres <- runQuery('Scripts/hawaiiProfile_TMI.sql', which='mysql')

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
  qres %>% filter(as.Date(signup_date)>='2017-01-01') %>% 
  count(campaign) %>% arrange(-n) %>% mutate(proportion = n/sum(n))

wb <- createWorkbook()

addWorksheet(wb, 'User')
addWorksheet(wb, 'campaign_type')
addWorksheet(wb, 'action_type')
addWorksheet(wb, 'cause_type')
addWorksheet(wb, 'campaigns')
writeData(wb, 'User', user, rowNames=F)
writeData(wb, 'campaign_type', campaign_type, rowNames=F)
writeData(wb, 'action_type', action_type, rowNames=F)
writeData(wb, 'cause_type', cause_type, rowNames=F)
writeData(wb, 'campaigns', campaign, rowNames=F)

saveWorkbook(
  wb, 
  paste0('Data/hawaiiMemberDescriptiveStats.xlsx'), 
  overwrite = TRUE
)