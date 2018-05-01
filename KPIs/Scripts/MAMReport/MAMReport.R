source('config/init.R')
library(scales)

mm <-
  runQuery('Scripts/MAMReport_usertypes.sql', 'pg')

qres <-
  runQuery('Scripts/MAMReport_actiontypes.sql', 'pg')

act <-
  qres %>%
  group_by(Month, niche) %>%
  mutate(
    prop = actions/sum(actions),
    Text = percent(prop),
    position = 1-cumsum(prop) + (.5*prop)
  )

act.N <-
  qres %>%
  group_by(month, action_type) %>%
  summarise(
    count = sum(count)
  ) %>%
  mutate(
    prop = count/sum(count),
    Text = percent(prop),
    position = 1-cumsum(prop) + (.5*prop)
  )

da <-
  runQuery('Scripts/MAMReport_dailyactive.sql', 'pg') %>%
  mutate(
    DayOfWeek = factor(weekdays(date),
                       levels=c('Monday','Tuesday','Wednesday','Thursday',
                                'Friday','Saturday','Sunday'))
  )