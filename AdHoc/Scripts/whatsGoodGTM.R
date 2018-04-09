source('config/init.R')

# ms <- 
#   runQuery('Scripts/whatsGoodGTM_mysql.sql', 'mysql') %>% 
#   mutate(
#     activeMoreThan1 = ifelse(active_jan+active_feb+active_march > 1, 1, 0)
  )

ms <- 
  read_csv('whatsgood_gtm_summary.csv') %>% 
  mutate(
    activeMoreThan1 = ifelse(active_jan+active_feb+active_march > 1, 1, 0)
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
  summarise_all(mean, na.rm=T) %>% 
  mutate(did_gtm = as.factor(did_gtm))

library(reshape2)

mout <- 
  melt(out, id.var='did_gtm', variable.name = 'Metric')

ggplot(mout, aes(x=did_gtm, y=value)) + 
  geom_bar(stat='identity') +
  facet_wrap(~Metric, scale = 'free_y') +
  theme(plot.title = element_text(hjust=.5)) +
  labs(title = 'Average Activity for Signups Since Jan 1st 2018', 
       x = 'Signed Up GtM')

ggplot(mout, aes(x=Metric, y=value, fill=did_gtm)) + 
  geom_bar(stat='identity', position='dodge') +
  theme(plot.title = element_text(hjust=.5)) +
  labs(title = 'Average Activity for Signups Since Jan 1st 2018', 
       x = 'Signed Up GtM')

