source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

q <- 
  "
SELECT *
FROM quasar.monitoring m
WHERE m.table='quasar.users' 
AND m.query='active_user_count'
"

use <- 
  runQuery(q, 'mysql') %>% 
  mutate(timestamp = as.POSIXct(timestamp, tz='EST', format='%Y-%m-%d %H:%M:%S')) %>% 
  filter(output > 0)

countMod <- lm(output ~ timestamp, use)

newDays <- 
  as.tibble(
    seq.Date(
      max(as.Date(use$timestamp)+1), 
      as.Date('2018-06-01'), 
      by='days') 
  ) %>%
  setNames('timestamp') %>% 
  mutate(timestamp = as.POSIXct(timestamp))

use %<>% 
  bind_rows(newDays) %>% 
  mutate(growth=output-lag(output))

use$p.output <- predict(countMod, use, type='response')

ggplot(use, aes(x=timestamp)) + 
  geom_line(aes(y=output)) +
  geom_line(aes(y=p.output), linetype='dotdash') +
  ggtitle('Engaged Member Growth') +
  scale_x_datetime(breaks=pretty_breaks(10)) + 
  theme(plot.title=element_text(hjust=0.5))

growthMod <- lm(growth ~ timestamp, use)
use$p.growth <- predict(growthMod, use, type='response')

ggplot(use, aes(x=timestamp, y=growth)) +
  geom_line() + geom_line(aes(y=p.growth), linetype='dotdash') + 
  geom_smooth() +
  scale_y_continuous(breaks=pretty_breaks(10)) +
  scale_x_datetime(breaks=pretty_breaks(10))
