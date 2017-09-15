source('config/init.R')
source('config/DSUtils.R')
source('config/mySQLConfig.R')
library(xlsx)
library(forecast)
library(scales)

#https://trello.com/c/aZZsQXiX/1127-sms-subscriber-growth-breakdown

q <- paste0(
  "SELECT 
  u.northstar_id,
  u.email,
  u.northstar_created_at_timestamp as created_date,
  u.northstar_id_source_name
  FROM quasar.users u 
  WHERE u.northstar_created_at_timestamp > '2008-01-01'
  AND u.moco_current_status = 'active'")

match <- 
  runQuery(q) %>%
  mutate(
    created_date = as.Date(created_date),
    createdMonth = firstDayOfMonth(created_date)
  )



source <- 
  match %>%
  group_by(createdMonth) %>%
  summarise(
    sign_ups = n()
  ) %>%
  arrange(createdMonth) %>%
  mutate(
    running_total = cumsum(sign_ups),
    monthOrder = as.numeric(as.factor(createdMonth))
  ) 

smsgrowth.M <- lm(
  running_total ~ monthOrder,
  source[createdMonth > '2014-01-01'],
  weights = monthOrder
)

smsgrowth.L <- lm(
  running_total ~ monthOrder,
  source[createdMonth > '2017-01-01'],
  weights = monthOrder
)

smsgrowth.H <- lm(
  running_total ~ monthOrder,
  source[createdMonth >= '2015-11-01' & createdMonth <= '2016-03-01'],
  weights = monthOrder
)

forecastDates <- 
  data.table(
    createdMonth = seq.Date(max(source$createdMonth), as.Date('2020-10-01'), 'months')
  ) %>%
  tbl_dt() %>% 
  filter(createdMonth != min(createdMonth)) %>% 
  mutate(
    monthOrder = seq(max(source$monthOrder)+1, length(unique(createdMonth))+max(source$monthOrder), 1)
  ) %>% 
  arrange(createdMonth) 

source %<>%
  bind_rows(forecastDates) 

source$predictTotal.Low <- predict(smsgrowth.L, newdata=source, type = 'response')
source$predictTotal.Medium <- predict(smsgrowth.M, newdata=source, type = 'response')
source$predictTotal.High.T <- predict(smsgrowth.H, newdata=source, type = 'response')

maxRT <- max(source$running_total, na.rm=T)
maxHigh <- max(source[createdMonth=='2017-09-01',predictTotal.High.T])
HighFactor <- maxHigh-maxRT

source %<>%
  rename('Month'='createdMonth') %>%
  filter(Month > '2014-01-01') %>% 
  mutate(
    predictTotal.High = predictTotal.High.T - HighFactor,
    predictTotal.Fixed = ifelse(Month > '2018-10-01', predictTotal.High + 500000, predictTotal.High),
    predictTotal.Kink = ifelse(Month > '2018-10-01', 
                               15000*monthOrder+predictTotal.High - 1881645, 
                               predictTotal.High)
    ) %>% 
  select(-predictTotal.High.T)


ggplot(source, aes(Month, running_total)) + 
  geom_line() +
  geom_line(aes(y=predictTotal.Medium), linetype = 'dotdash') + 
  geom_line(data=source[Month > '2017-08-01'],aes(y=predictTotal.Low), linetype = 'dotted', color='red') + 
  geom_line(data=source[Month > '2017-08-01'],aes(y=predictTotal.High), linetype = 'dotted', color='blue') + 
  geom_line(data=source[Month > '2017-08-01'],aes(y=predictTotal.Fixed), linetype = '12345678') + 
  geom_line(data=source[Month > '2017-10-01'],aes(y=predictTotal.Kink), linetype = 'longdash') + 
  labs(x='Date', y='Running Total of Registrations', title='Active SMS Projections') +
  scale_y_continuous(breaks = pretty_breaks(n=10)) + scale_x_date(breaks = pretty_breaks(n=10))

saveCSV(source, desktop=T)
