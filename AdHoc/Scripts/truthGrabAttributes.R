# https://trello.com/c/ByvAgZbI/1198-truth-sms-opt-ins-csv
source('config/init.R')
library(jsonlite)

json <- fromJSON('Data/truthSignups.json')

tr <- 
  read_csv('Data/truthSignups.csv') %>% 
  filter(grepl('platform', X2)) %>% 
  mutate(
    phone_number = cleanPhone(X2)
  ) %>% 
  select(phone_number)

q <-
  paste0(
    "SELECT 
      u.northstar_id,
      COALESCE(NULLIF(i.mobile, ''), NULLIF(u.mobile, '')) as phone_number
    FROM quasar.users u
    LEFT JOIN quasar.moco_profile_import i ON i.moco_id = u.moco_commons_profile_id
    WHERE COALESCE(NULLIF(i.mobile, ''), NULLIF(u.mobile, '')) IS NOT NULL"
  )

qres <- runQuery(q, 'mysql')

result <- 
  qres %>% 
  mutate(phone_number = cleanPhone(phone_number)) %>% 
  filter(phone_number %in% tr$phone_number) %>% 
  select(northstar_id)

nsids <- prepQueryObjects(result$northstar_id)

q <- 
  paste0(
    "SELECT 
  	  u.northstar_id,
      u.mobile, 
      u.email,
      u.first_name,
      u.last_name,
      u.birthdate,
      COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state
    FROM quasar.users u
    LEFT JOIN quasar.moco_profile_import m
    ON m.moco_id = u.moco_commons_profile_id
    WHERE u.northstar_id IN",nsids
  )

features <- runQuery(q, 'mysql')

saveCSV(features, desktop=T)
