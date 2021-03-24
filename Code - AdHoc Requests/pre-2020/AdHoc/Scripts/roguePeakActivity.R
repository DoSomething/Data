q <- 
  "SELECT 
    c.signup_id,
    c.signup_created_at,
    c.post_id,
    c.submission_created_at
  FROM quasar.campaign_activity c 
  "

qres <- runQuery(q, 'mysql')

signups <-
  qres %>% 
  filter(!duplicated(signup_id)) %>% 
  select(signup_id, signup_created_at) %>% 
  mutate(
    signup_created_at = as.POSIXct(signup_created_at, '%Y-%m-%d %H:%M:%S', tz='UTC'),
    dayOfWeek = factor(
      weekdays(signup_created_at),
      levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
      ),
    month = as.numeric(month(signup_created_at)),
    dayOfMonth = as.numeric(substr(signup_created_at, 9, 10)),
    Hour = as.numeric(substr(signup_created_at, 12, 13)),
    Type = 'Signup'
  ) %>% 
  filter(!is.na(signup_created_at)) %>% 
  rename(
    id = signup_id,
    timestamp = signup_created_at
  ) %>% 
  mutate(id = as.character(id))

posts <-
  qres %>% 
  filter(post_id != -1 & !duplicated(post_id)) %>% 
  select(post_id, submission_created_at) %>% 
  mutate(
    submission_created_at = as.POSIXct(submission_created_at, '%Y-%m-%d %H:%M:%S', tz='UTC'),
    dayOfWeek = factor(
      weekdays(submission_created_at),
      levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
    ),
    month = as.numeric(month(submission_created_at)),
    dayOfMonth = as.numeric(substr(submission_created_at, 9, 10)),
    Hour = as.numeric(substr(submission_created_at, 12, 13)),
    Type = 'Post'
  ) %>% 
  filter(!is.na(submission_created_at)) %>% 
  rename(
    id = post_id,
    timestamp = submission_created_at
  )

dat <- bind_rows(signups, posts)

ggplot(dat, aes(x=dayOfWeek)) +
  geom_bar(stat='count') + 
  facet_wrap(~Type, scales='free_y') + 
  ggtitle('Day Of Week')+ 
  theme(plot.title=element_text(hjust=.5))

ggplot(dat, aes(x=dayOfMonth)) +
  geom_bar(stat='count') + 
  facet_wrap(~Type, scales='free_y') + 
  ggtitle('Day Of Month') + 
  theme(plot.title=element_text(hjust=.5))

ggplot(dat, aes(x=Hour)) +
  geom_bar(stat='count') + 
  facet_wrap(~Type, scales='free_y') + 
  ggtitle('Hour Of Day')+ 
  theme(plot.title=element_text(hjust=.5))

ggplot(dat, aes(x=month)) +
  geom_bar(stat='count') + 
  facet_wrap(~Type, scales='free_y') + 
  ggtitle('Month Of Year')+ 
  theme(plot.title=element_text(hjust=.5)) + 
  scale_x_continuous(breaks=seq(1,12,1))
