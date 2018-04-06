source('config/init.R')

ms <- 
  runQuery('Scripts/whatsGoodGTM_mysql.sql', 'mysql') %>% 
  mutate(
    activeMoreThan1 = ifelse(active_jan+active_feb+active_march+active_april > 1, 1, 0)
  )

pg <- runQuery('Scripts/whatsGoodGTM_pg.sql', 'pg')

sesh <- 
  pg %>% 
  filter(nchar(northstar_id)>20) %>% 
  group_by(northstar_id) %>% 
  summarise(
    time_on_site = sum(difftime(ending_ts, landing_ts, units='mins'))
  )

setwd('../KPIs/')
today = Sys.Date()-4
source('../KPIs/Scripts/VoterReg/turbovoteFile.R')
setwd('../AdHoc/')

vr_stat <- 
  vr %>% 
  select(nsid, ds_vr_status) %>% 
  rename(northstar_id = nsid)

out <- 
  ms %>% 
  left_join(sesh) %>% 
  left_join(vr_stat) %>% 
  mutate(registered = ifelse(grepl('register', ds_vr_status), 1, 0)) %>% 
  select(-northstar_id, -ds_vr_status) %>% 
  group_by(did_gtm) %>%
  summarise_all(mean, na.rm=T)
