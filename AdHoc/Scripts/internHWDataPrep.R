library(openssl)
t <- 
  read_csv('~/Downloads/member_event_log 2018-03-05T1612.csv') %>%
  setNames(c('event_id','user_id','action_type','channel','date')) %>% 
  filter(date >= '2017-06-01') %>% 
  mutate(
    channel = ifelse(grepl('niche', tolower(channel)), 'paid',
                     ifelse(grepl('sms', channel), 'sms', 'web')),
    timestamp = substr(event_id, 25, 43) %>% 
      as.POSIXct(format='%Y-%m-%d %H:%M:%S'),
    event_id = md5(event_id)
  ) %>% select(-date)

saveCSV(t, desktop=T)
