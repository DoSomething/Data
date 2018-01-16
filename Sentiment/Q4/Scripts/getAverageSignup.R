source('config/init.R')
source('config/mySQLConfig.R')

nsids2016 <-
  nps.2016 %>%
  filter(!is.na(northstar_id)) %>%
  select(northstar_id) %>%
  unlist() %>% prepQueryObjects()

yearAgo <- as.Date('2016-12-05') - 365

q <-
  paste0(
    "SELECT
      c.northstar_id,
      c.signup_created_at
    FROM quasar.campaign_activity c
    WHERE c.signup_created_at >= '",yearAgo,"'",
    " AND c.signup_created_at < '2016-12-05'",
    " AND c.northstar_id in ",  nsids2016
  )

qres <- runQuery(q, which='mysql')

minDate <- min(as.Date(substr(qres$signup_created_at, 1, 10)))

dat <-
  qres %>%
  group_by(northstar_id) %>%
  summarise(
    avg_signup_date = as.Date(mean(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01')
  ) %>%
  mutate(
    dateCounter = as.numeric(avg_signup_date) - as.numeric(minDate) + 1,
    scaleDates = scalerange(dateCounter)
  )

peak <-
  dat %>%
  filter(avg_signup_date==as.Date('2016-12-04')-60) %>%
  select(scaleDates) %>% unique() %>% as.numeric()

probs <-
  dat %>%
  mutate(
    prob = 1 - abs(scaleDates - peak)
  ) %>%
  select(northstar_id, prob)
