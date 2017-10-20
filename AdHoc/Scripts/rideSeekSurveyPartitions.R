source('config/init.R')
source('config/mySQLConfig.R')

q <- "
SELECT DISTINCT
	u.northstar_id,
  u.mobile
FROM quasar.users u
INNER JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
WHERE c.signup_created_at >= '2017-01-01'
AND u.mobile IS NOT NULL
"

qres <- runQuery(q)

rs <- 
  read_csv('Data/outgoing_messages_rideandseek2017_oct17_final_20171018_173333.csv') %>% 
  select(phone_number) %>% 
  mutate(
    rand = runif(nrow(.), 0, 1),
    rand1 = runif(nrow(.), 0, 1),
    mobile = as.character(phone_number),
    group = if_else(rand > .5, '10-19',
                    if_else(rand > .25, '10-21',
                            if_else(rand > .125, '10-25',
                                    if_else(rand > .065, '11-02',
                                            if_else(rand1 > .5, '11-17', '12-11'))))),
    type = 'Experiment'
  ) %>% 
  select(mobile, group, type)

signs2017 <- 
  qres %>% 
  filter(!(mobile %in% rs$mobile) & mobile != '') %>% 
  mutate(
    rand = runif(nrow(.), 0, 1),
    rand1 = runif(nrow(.), 0, 1),
    rand2 = runif(nrow(.), 0, 1),
    group = if_else(rand > .5, '10-19',
                    if_else(rand > .25, '10-21',
                            if_else(rand > .125, '10-25',
                                    if_else(rand > .065, '11-02',
                                            if_else(rand1 > .5, '11-17', '12-11'))))),
    type = 'Control'
  ) %>% 
  sample_n(15000) %>% 
  select(mobile, group, type)

out <- rs %>% bind_rows(signs2017) %>% arrange(type, group)

saveCSV(out)

