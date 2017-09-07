source('config/init.R')
source('config/DSUtils.R')
source('config/mySQLConfig.R')
library(xlsx)
library(forecast)
library(scales)

#https://trello.com/c/aZZsQXiX/1127-sms-subscriber-growth-breakdown
# leads <- 
#   read_csv('Data/DoSomething.org_-_July_2017_Leads.csv') %>%
#   select(email) %>%
#   filter(!duplicated(email))

# ems <- prepQueryObjects(leads$email)

q <- paste0(
  "SELECT 
    u.northstar_id,
  u.email,
  u.northstar_created_at_timestamp as created_date,
  u.northstar_id_source_name
  FROM quasar.users u 
  WHERE u.northstar_created_at_timestamp > '2008-01-01'")

web <- c('drupal', 'phoenix', 'phoenix-next','phoenix-oauth','cgg','voting_app')
sms <- c('message_broker','sms')
app <- c('android','letsdothis-ios','mobileapp_android','mobileapp_ios')
niche <- 'niche'
other <- c('northstar','aurora','quasar-etl-node','test-source')
exclude <- c('runscope','runscope-client','')

match <- 
  runQuery(q) %>%
  mutate(
    created_date = as.Date(created_date),
    source = ifelse(northstar_id_source_name %in% web, 'web',
                    ifelse(northstar_id_source_name %in% sms, 'sms',
                           ifelse(northstar_id_source_name %in% app, 'app',
                                  ifelse(northstar_id_source_name %in% niche, 'niche',
                                         ifelse(northstar_id_source_name %in% other, 'other', 'exclude')))))
  )

# kikalist <-
#   match %>%
#   filter(
#     email %in% leads$email
#   )

source <- 
  match %>%
  group_by(created_date, source) %>%
  summarise(
    sign_ups = n()
  ) %>%
  arrange(created_date, source) %>%
  group_by(source) %>%
  mutate(
    running_total = cumsum(sign_ups)
  ) %>%
  ungroup(source) %>%
  filter(
    !(source %in% c('exclude','other','app'))
  ) %>%
  mutate(
    daysSinceBeginning = as.numeric(created_date - min(created_date))
  ) %>% 
  group_by(created_date) %>%
  mutate(
    proportion_signups = sign_ups / sum(sign_ups)
  ) %>% 
  ungroup(created_date)

ggplot(source, aes(created_date, proportion_signups, source)) + 
  geom_smooth(aes(color=source)) + 
  ggtitle('Proportion Signups Over Time') + 
  scale_y_continuous(breaks=pretty_breaks(10)) +
  scale_x_date(breaks=pretty_breaks(10))

smsgrowth <- lm(
  running_total ~ daysSinceBeginning + source,
  source[!is.na(running_total)],
  weights = daysSinceBeginning
  )

# glm(proportion_signups ~ created_date, family='binomial', data=source, weights=)

forecastDates <- 
  expand.grid(
    created_date = seq.Date(max(source$created_date), as.Date('2018-09-30'), 'days'),
    source = unique(source$source)
  ) %>%
  tbl_dt() %>%
  arrange(created_date, source) %>%
  filter(
    !(source %in% c('exclude','other','app'))
  )

source %<>%
  bind_rows(forecastDates) %>% 
  mutate(
    daysSinceBeginning = as.numeric(created_date - min(created_date))
  )

source$predictTotal <- predict(smsgrowth, newdata=source, type = 'response')

ggplot(source[created_date < '2017-09-02'], aes(created_date, running_total, source)) + 
  geom_smooth(aes(color=source), size = .5, se=F) +
  geom_line(data=source[created_date < '2018-06-01'],aes(y=predictTotal, color=source), linetype='dotdash') +
  labs(x='Date', y='Running Total of Registrations', title='Registration Extrapolations') +
  scale_y_continuous(breaks = pretty_breaks(n=10)) + scale_x_date(breaks = pretty_breaks(n=10)) + 
  ylim(0,5000000)

