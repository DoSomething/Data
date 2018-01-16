# https://trello.com/c/MVbkClJB/1196-truth-opt-in-users
source('config/init.R')

opt_ins <- read_csv('Data/truth_opt_ins.csv')

q <-
  "
SELECT
  c.signup_id,
  u.northstar_id,
  u.created_at,
  u.birthdate,
  u.addr_state,
  c.campaign_run_id,
  max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) as reportbacks
FROM quasar.users u
INNER JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
WHERE c.campaign_run_id IN (7651, 7979)
AND c.signup_source NOT LIKE '%sms%' 
AND c.signup_source <> ''
GROUP BY c.northstar_id
"

topt <- runQuery(q, 'mysql')

pop <- 
  topt %>% 
  inner_join(opt_ins) %>% 
  mutate(
    created_at = as.Date(as.POSIXct(created_at)),
    birthdate = as.Date(birthdate),
    age = floor((Sys.Date() - birthdate)/365.25),
    daysMember = Sys.Date() - created_at
  ) %>%  
  group_by(campaign_run_id) %>% 
  summarise(
    age = as.numeric(mean(age, na.rm=T)),
    daysMember = mean(daysMember),
    rbRate = mean(reportbacks)
  ) %>% 
  mutate(
    campaign_name = if_else(campaign_run_id == 7651, 'WHTEOY', 'MBR'),
    nps = if_else(campaign_run_id == 7651, 22, 12)
  )
