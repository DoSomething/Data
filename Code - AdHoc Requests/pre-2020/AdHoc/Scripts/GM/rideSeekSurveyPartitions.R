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
    group = case_when(rand > .5 ~ '10-19',
                      rand > .25 ~ '10-21',
                      rand > .125 ~ '10-25',
                      rand > .065 ~ '11-02',
                      rand1 > .5 ~ '11-17', 
                      is.numeric(rand) ~ '12-11'),
    type = 'Control'
  ) %>% 
  sample_n(15000) %>% 
  select(northstar_id, mobile, group, type)

out <- rs %>% bind_rows(signs2017) %>% arrange(type, group)

saveCSV(out)

rs <- 
  read_csv('Data/outgoing_messages_rideandseek2017_oct17_final_20171018_173333.csv') %>% 
  select(phone_number) %>% 
  mutate(mobile = cleanPhone(phone_number)) %>% 
  left_join(
    qres %>% mutate(mobile=cleanPhone(mobile))
  )


# Recipent NSIDs ----------------------------------------------------------

recips <- 
  read_csv('Data/Survey_Recipients.csv') %>% 
  filter(group %in% c('11-02', '11-17', '12-11')) %>% 
  left_join(qres)

missing <- 
  recips %>% 
  filter(is.na(northstar_id))

have <- 
  recips %>% 
  filter(!is.na(northstar_id))

naPhone <- prepQueryObjects(missing$mobile)

q <-
  paste0(
    "SELECT
      u.northstar_id,
      m.mobile
     FROM quasar.moco_profile_import m
     LEFT JOIN quasar.users u ON m.moco_id = u.moco_commons_profile_id
     WHERE u.moco_commons_profile_id IS NOT NULL
     AND m.mobile IN 
    ", naPhone
  )

missingNSIDS.q <- runQuery(q)

both <- 
  missing %>% 
  select(mobile, group, type) %>% 
  left_join(missingNSIDS.q) %>% 
  bind_rows(have)

extraCont <- 
  signs2017 %>% 
  filter(group %in% c('11-02', '11-17', '12-11')) %>% 
  sample_n(1000)
  
secondSend <- 
  both %>% 
  bind_rows(extraCont) %>% 
  filter(!is.na(northstar_id))

saveCSV(secondSend)


# Review Response Groups --------------------------------------------------

resp <- 
  read_csv('~/Downloads/Ride and Seek - 10-19-2017_sh.csv') %>% 
  select(`Response ID`, `16 - Q7`) %>% 
  transmute(id=`Response ID`, mobile=cleanPhone(`16 - Q7`)) 
  

r1 <- read_csv('Survey_Recipients.csv')
r2 <- read_csv('Survey_Recipients_v2.csv')

r <- bind_rows(
  mutate(r1, mobile = cleanPhone(mobile)), 
  mutate(r2, mobile = cleanPhone(mobile))
  )

matched <- 
  r %>% 
  filter(mobile %in% resp$mobile)


# More Control ------------------------------------------------------------

q <-
    "SELECT DISTINCT
      u.northstar_id,
      COALESCE(m.mobile, u.mobile) as mobile
    FROM quasar.users u
    LEFT JOIN quasar.moco_profile_import m ON m.moco_id = u.moco_commons_profile_id
    INNER JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
    WHERE COALESCE(m.mobile, u.mobile) IS NOT NULL
    AND c.signup_created_at >= '2017-01-01' 
    AND c.campaign_run_id <> 7931
    AND RAND() < .01
    LIMIT 12000"

getControl <- runQuery(q, 'mysql')
saveCSV(getControl, desktop=T)
