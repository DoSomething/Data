source('config/init.R')
library(scales)

mm <-
  runQuery('Scripts/MAMReport/usertypes.sql', 'pg') %>%
  filter(Month >= '2017-01-01')

qres <-
  runQuery('Scripts/MAMReport/actiontypes.sql', 'pg') %>%
  filter(Month >= '2017-01-01')

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
  ) %>%
  filter(date >= '2017-01-01')

actMemAvg <-
  runQuery('Scripts/MAMReport/avgActionsPerMember.sql', 'pg') %>%
  filter(Month >= '2017-01-01')

tos <-
  runQuery('Scripts/MAMReport/timeOnSite.sql','pg') %>%
  mutate(
    sessionLength = difftime(ending_ts, landing_ts, unit='mins'),
    date = as.Date(landing_ts)
    ) %>%
  filter(date < today()) %>%
  group_by(date, northstar_id) %>%
  summarise(
    timeThatDay = sum(sessionLength)
  ) %>%
  filter(
    timeThatDay > quantile(timeThatDay, .001) &
    timeThatDay < quantile(timeThatDay, .999)
  ) %>%
  group_by(date) %>%
  summarise(
    avgTimeOnSite = mean(timeThatDay)
  )
