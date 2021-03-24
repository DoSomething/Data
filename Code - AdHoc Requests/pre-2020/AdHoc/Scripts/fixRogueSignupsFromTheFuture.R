source('config/init.R')
source('config/mySQLConfig.R')

q <- 
  "
SELECT 
c.signup_id,
c.campaign_run_id,
c.signup_created_at
FROM quasar.campaign_activity c
WHERE 
c.campaign_run_id IN 
(SELECT DISTINCT ca.campaign_run_id
FROM quasar.campaign_activity ca 
WHERE ca.signup_created_at >= now())
"

qres <- runQuery(q, which='mysql')

q <- 
  "
SELECT 
c.signup_id,
c.campaign_run_id
FROM quasar.campaign_activity c
WHERE c.signup_created_at >= now()
"

toFix <- 
  runQuery(q, which='mysql') %>% 
  arrange(campaign_run_id) %>% 
  bind_rows(
    read_csv('~/Downloads/all_pre_gambit_sms_signups_with_campaign_id_and_northstar.csv') %>% 
      filter(signup_created_at_timestamp == max(signup_created_at_timestamp, na.rm=T)) %>% 
      transmute(
        campaign_run_id = campaign_run_id, 
        signup_id = drupal_id
      )
  )

counts <- 
  toFix %>% 
  group_by(campaign_run_id) %>% 
  summarise(Count = n())

runs <- unique(qres$campaign_run_id)

outFrame <- tibble()
for (i in 1:length(runs)) {
  howMany <- 
    counts %>% 
    filter(campaign_run_id==runs[i]) %>% 
    select(Count) %>% as.numeric()
  
  vals <- 
    qres %>% 
    filter(campaign_run_id==runs[i]) %>% 
    sample_n(howMany) %>% 
    select(-signup_id)
  
  outFrame <- 
    outFrame %>% bind_rows(vals)
  
}

out <- 
  outFrame %>% 
  arrange(campaign_run_id) %>% 
  bind_cols(toFix) %>% 
  select(signup_id, signup_created_at)

saveCSV(out)
