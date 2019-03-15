source('config/init.R')
library(broom)

q <- sprintf("
SELECT
  u.created_at,
  mel.event_id,
  mel.timestamp,
  mel.northstar_id,
  mel.action_type
FROM users u
INNER JOIN member_event_log mel ON mel.northstar_id = u.northstar_id
WHERE mel.timestamp >= '2014-01-01'
AND mel.timestamp <= %s", sQuote(Sys.Date()))

read <- runQuery(q)
# read <-
#   read_csv('Data/mel_2017-10-02_2016-10-01.csv') %>%
#   setNames(c('email','northstar_id','action_type','event_id','ts','northstar_created')) %>%
#   mutate(event_id = seq(1,nrow(.), 1))

mel <-
  read %>%
  mutate(
    daysSinceCreated = timestamp - created_at
  ) %>%
  filter(
    timestamp > '1970-01-01'
  )

ggplot(filter(mel, daysSinceCreated>=0), aes(x=daysSinceCreated)) + geom_density()

northstarCount <-
  mel %>%
  filter(daysSinceCreated > 0) %>%
  summarise(length(unique(northstar_id))) %>%
  as.numeric()

actionRates <-
  mel %>%
  filter(daysSinceCreated > 0) %>%
  group_by(daysSinceCreated) %>%
  summarise(
    percentActive = n() / northstarCount
  ) %>%
  mutate(
    previousRate = lag(percentActive)
  )

actionMod <-
  lm(log(percentActive) ~ daysSinceCreated + previousRate, data=actionRates)
glance(actionMod)

actionRates$predictActive <- exp(predict(actionMod, actionRates, type="response"))

ggplot(actionRates, aes(x=daysSinceCreated)) +
  geom_line(aes(y=percentActive)) +
  geom_line(aes(y=predictActive, color='red')) +
  labs(
    x='Days Since Registration',
    y='Percent of Users Active',
    title='Predicted & Actual User Activity over Lifetime'
    ) +
  theme(plot.title=element_text(hjust = 0.5), legend.position='none') +
  scale_x_continuous(breaks=pretty_breaks(40))
