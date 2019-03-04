source('config/init.R')
library(glue)

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

q <- 
  "SELECT 
    northstar_id,
    source,
    source_detail,
    created_at,
    birthdate
  FROM users
  WHERE subscribed_member=true"

qres <- runQuery(q)

dat <- 
  qres %>% 
  mutate(
    membership_length = age(created_at, units='months')/12,
    mlengthDays = age(created_at, units='days'),
    lengthBuckets = 
      case_when(
        mlengthDays < 90 ~ '<90',
        mlengthDays < 365 ~ '90-365 days',
        mlengthDays < 730 ~ '1-2 yrs',
        mlengthDays < 1095 ~ '2-3 yrs',
        mlengthDays < 1460 ~ '3-4 yrs',
        mlengthDays >= 1460 ~ '4+ yrs'
      ),
    channel = case_when(!source %in% c('sms','niche') ~ 'web',
                        TRUE ~ source),
    age = age(birthdate)
  )

sumstats <- 
  dat %>% 
  filter(membership_length<8) %>% 
  group_by(channel) %>% 
  summarise(
    avgLengthMembership = mean(membership_length),
    avgAge = mean(age, na.rm = T)
  )

dat %>% 
  filter(channel=='sms') %>% 
  count(lengthBuckets) %>% 
  mutate(pct=n/sum(n))

dat %>% 
  filter(channel=='sms') %>% 
  count(source_detail) %>% 
  arrange(-n) 

quantile(dat %>% filter(channel=='sms') %$% membership_length, seq(.05,.95,.05))

dat %>% 
  filter(channel=='sms' & source_detail=='tell_a_friend') %>% 
  mutate(
    monthYear = as.Date(paste0(substr(created_at, 1, 7),'-01'))
  ) %>% 
  count(monthYear) %>% 
  filter(monthYear >= '2011-01-01') %>% 
  ggplot(.,aes(x=monthYear, y=n)) + 
  geom_line() +
  scale_x_date(breaks = pretty_breaks(20)) +
  ggtitle('SMS Tell a Friend Accounts Created')

ggplot(filter(dat, membership_length<8), aes(x=channel, y=membership_length)) +
  geom_violin(aes(fill=channel)) +
  scale_y_continuous(breaks=seq(0,7,1)) +
  coord_flip()

ggplot(filter(dat, age<40), aes(x=age, fill=channel)) + 
  geom_bar(stat='count', position='dodge')

q <- "SELECT DISTINCT northstar_id 
FROM member_event_log 
WHERE timestamp >= '2019-01-01'"

qres <- runQuery(q)

dat %>%
  filter(northstar_id %in% qres$northstar_id) %>% 
  count(lengthBuckets) %>% 
  mutate(pct=n/sum(n))

q <- 
  "SELECT 
    u.northstar_id,
    u.source,
    u.source_detail,
    u.created_at,
    u.sms_status,
    count(*) AS n_lifetime_actions
  FROM users u
  INNER JOIN member_event_log mel ON mel.northstar_id = u.northstar_id
  WHERE sms_status IN ('active','less','pending','stop')
  GROUP BY u.northstar_id, u.source, u.source_detail, u.created_at, u.sms_status"

qres <- runQuery(q)

qres %>% 
  mutate(
    gamer = case_when(source_detail=='tell_a_friend' ~ 'sms_gamer',
                      TRUE ~ 'non-sms_gamer')
  ) %>% 
  group_by(gamer) %>% 
  summarise(
    n = n(),
    avgActions = mean(n_lifetime_actions)
  )
