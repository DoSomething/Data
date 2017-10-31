source('config/init.R')
source('config/mySQLConfig.R')
library(broom)

q <- "
SELECT 
  date(u.northstar_created_at_timestamp) AS date,
  count(*) AS signups
FROM quasar.users u 
WHERE (u.customer_io_subscription_status = 'subscribed' 
       OR u.moco_current_status = 'active')
GROUP BY date(u.northstar_created_at_timestamp)
ORDER BY date(u.northstar_created_at_timestamp)
"

qres <- runQuery(q)

signupsDay <-
  qres %>% 
  mutate(
    runningTotal = cumsum(signups),
    date = as.Date(date)
  ) %>% 
  filter(date > '2012-01-01')

memberMod <- lm(runningTotal ~ date, signupsDay)

signupsDay %<>%
  bind_rows(
    data.frame(
      date=seq.Date(max(signupsDay$date)+1, as.Date('2020-12-30'), by = 1))
  ) 

signupsDay$expectMembers <- predict(memberMod, signupsDay, type='response')

nicheAddition <- 200000*.34
base <- signupsDay %>% 
  filter(date=='2017-10-31') %>% 
  select(expectMembers) %>% as.numeric()

signupsDay %<>%
  mutate(
    expectMembers = if_else(date >= '2018-01-01', 
                            expectMembers + nicheAddition, 
                            expectMembers),
    memberIndex = expectMembers / base
  ) %>% 
  filter(date >= '2017-01-01')

ggplot(signupsDay, aes(date)) + 
  geom_line(aes(y=runningTotal)) + 
  geom_line(aes(y=expectMembers), linetype='dotdash')

reg2017 <- 
  read_csv('Data/registrations_day_2017.csv') %>% 
  mutate(
    date = as.Date(date, '%m/%d/%y'),
    runningTotal = cumsum(registrations)
    )

ggplot(reg2017, aes(date, runningTotal)) + geom_line() + geom_smooth(method='lm')

regMod <- lm(runningTotal ~ date, reg2017)

signupsDay$vcardReg <- predict(regMod, signupsDay, type = 'response')

blastMonths <- c(1,3,5,7,9,11)

eoy <-
  signupsDay %>%
  mutate(
    blastReg = cumsum(if_else(month(date) %in% blastMonths & date==firstDayOfMonth(date), 3000, 0)),
    totalReg = (blastReg + vcardReg)*memberIndex
  ) %>% 
  # filter(date %in% as.Date(c('2018-12-30','2019-12-30','2020-12-30'))) %>% 
  mutate(
    yearIndex = if_else(year(date)==2019, .75, 1),
    diminishingReturn = case_when(year(date)==2018 ~ 1,
                                  year(date)==2019 ~ .9,
                                  year(date)==2020 ~ .8)
  ) %>% 
  group_by(year(date)) %>% 
  select(-signups, -runningTotal)
