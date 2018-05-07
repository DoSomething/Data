source('config/init.R')
library(scales)

mm.old <-
  read_csv('Data/MAM_breakdown_April.csv') %>%
  setNames(c('month','return_user','niche','nsids')) %>%
  mutate(
    niche = ifelse(niche=='Yes', 'Niche', 'Non-Niche'),
    Type = ifelse(return_user=='Yes', 'Returning', 'New'),
    Month = as.Date(paste0(month, '-01'))
  ) %>% select(-month, -return_user)

mm <-
  runQuery('Scripts/MAMReport/usertypes.sql', 'pg') %>%
  filter(Month >= '2017-06-01' & Month < '2018-05-01') %>%
  bind_rows(mm.old) %>%
  arrange(Month, Type, niche)

act.old <-
  read_csv('Data/MAM_actionbreakdown_April.csv') %>%
  setNames(c('Month', 'action_type', 'niche', 'actions')) %>%
  mutate(
    niche = ifelse(niche=='Yes', 'Niche', 'Non-Niche'),
    Month = as.Date(paste0(Month,'-01'))
  )

qres <-
  runQuery('Scripts/MAMReport/actiontypes.sql', 'pg') %>%
  filter(Month >= '2017-01-01' & Month < '2018-05-01') %>%
  filter(Month >= '2017-06-01' | !(action_type %in% c('site_login','site_access'))) %>%
  bind_rows(act.old)

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
  filter(date >= '2017-06-01')

actMemAvg <-
  runQuery('Scripts/MAMReport/avgActionsPerMember.sql', 'pg') %>%
  filter(Month >= '2017-06-01')

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
