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

ggplot(filter(dat, membership_length<8), aes(x=channel, y=membership_length)) +
  geom_violin(aes(fill=channel)) +
  scale_y_continuous(breaks=seq(0,7,1)) +
  coord_flip()

ggplot(filter(dat, age<40), aes(x=age, fill=channel)) + 
  geom_bar(stat='count', position='dodge')
