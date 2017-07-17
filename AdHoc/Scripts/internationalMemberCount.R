chimp <- tbl_dt(read_csv('~/Downloads/subscribed_members_export_33d3bb2d7a.csv'))
unsub <- tbl_dt(read_csv('~/Downloads/unsubscribed_members_export_33d3bb2d7a.csv'))

unsub.sub <- unsub %>% select(`Email Address`, `CONFIRM_TIME`, `UNSUB_TIME`) %>% setNames(c('email','confirm_time_unsub','unsub_time'))
chimpSub <- chimp %>% select(`Email Address`, CONFIRM_TIME, LAST_CHANGED) %>% setNames(c('email', 'confirm_time', 'last_changed'))

both <- 
  chimpSub %>%
  left_join(unsub.sub, by='email')

unsub.sub.unaltered <- unsub %>% select(`Email Address`, `CONFIRM_TIME`, `UNSUB_TIME`)
chimpSub.unaltered <- chimp %>% select(`Email Address`, CONFIRM_TIME, LAST_CHANGED)

unioned <- rbind(unsub.sub.unaltered, chimpSub.unaltered, fill=T)

agg <- unioned[,.(count = .N), by=year(CONFIRM_TIME)][order(year)][,runningTotal := cumsum(count)]
