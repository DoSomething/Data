source('config/init.R')
library(scales)

mm <-
  runQuery('Scripts/MAMReport/usertypes.sql', 'pg') %>%
  filter(Month > '2017-01-01' & Month < '2018-05-01')

qres <-
  runQuery('Scripts/MAMReport/actiontypes.sql', 'pg') %>%
  filter(Month > '2017-01-01' & Month < '2018-05-01')

act <-
  qres %>%
  group_by(Month, niche) %>%
  mutate(
    prop = actions/sum(actions),
    Text = percent(prop)
  )

act.N <-
  qres %>%
  group_by(Month, action_type) %>%
  summarise(
    count = sum(actions)
  ) %>%
  mutate(
    prop = count/sum(count),
    Text = percent(prop)
  )

da <-
  runQuery('Scripts/MAMReport/dailyactive.sql', 'pg') %>%
  mutate(
    DayOfWeek = factor(weekdays(date),
                       levels=c('Monday','Tuesday','Wednesday','Thursday',
                                'Friday','Saturday','Sunday'))
  )
