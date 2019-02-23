source('config/init.R')
library(glue)
library(eeptools)

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
  mutate(n/sum(n))

quantile(dat %>% filter(channel=='sms') %$% membership_length, seq(.05,.95,.05))

ggplot(filter(dat, membership_length<8), aes(x=channel, y=membership_length)) +
  geom_violin(aes(fill=channel)) +
  scale_y_continuous(breaks=seq(0,7,1)) +
  coord_flip()

ggplot(filter(dat, age<40), aes(x=age, fill=channel)) + 
  geom_bar(stat='count', position='dodge')
