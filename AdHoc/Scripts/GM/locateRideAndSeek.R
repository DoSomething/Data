source('config/init.R')
source('config/mySQLConfig.R')
library(zipcode)
library(choroplethr)
library(choroplethrZip)

q <- "SELECT distinct
	u.northstar_id,
	a.signup_id,
	UNIX_TIMESTAMP(u.northstar_created_at_timestamp) AS created_at,
	DATE(u.birthdate) AS birthdate,
	u.first_name,
	u.last_name,
	u.`language`,
	COALESCE(NULLIF(m.mobile,''), NULLIF(u.mobile,'')) as mobile,
  NULLIF(u.mobile,'') as users_mobile,
	m.status AS sms_status,
	u.northstar_id_source_name AS source,
	COALESCE(NULLIF(u.addr_street1, ''), NULLIF(m.addr_street1, '')) AS addr_street1,
	COALESCE(NULLIF(u.addr_street2, ''), NULLIF(m.addr_street2, '')) AS addr_street2,
	COALESCE(NULLIF(u.addr_city,''), NULLIF(m.addr_city,''), NULLIF(m.loc_city,'')) AS addr_city,
	COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state,
	COALESCE(NULLIF(u.addr_zip, ''), NULLIF(m.addr_zip, ''), NULLIF(m.loc_zip,'')) AS addr_zip,
	COALESCE(NULLIF(u.country, ''), NULLIF(m.addr_country,''), NULLIF(m.loc_country,'')) AS addr_country
FROM quasar.campaign_activity a
LEFT JOIN quasar.users u 
	ON a.northstar_id = u.northstar_id
LEFT JOIN quasar.moco_profile_import m 
  ON (m.moco_id = u.moco_commons_profile_id AND u.moco_commons_profile_id IS NOT NULL)
WHERE a.campaign_run_id = '7931'"

qres <- runQuery(q, which='mysql')

c <- 
  qres %>% 
  group_by(mobile) %>% 
  mutate(
    zip = clean.zipcodes(max(addr_zip, na.rm=T))
  ) %>% 
  ungroup()

pctMissing <- sum(is.na(c$zip)) / nrow(c)

zipCount <-
  c %>% 
  group_by(zip) %>% 
  summarise(
    n = n()
  ) %>% 
  filter(!is.na(zip)) %>% 
  mutate(
    Count = ifelse(
      applyPctChange(n, pctMissing)-floor(applyPctChange(n, pctMissing)) > .2, 
      ceiling(applyPctChange(n, pctMissing)), round(applyPctChange(n, pctMissing))
      )
  ) %>% 
  arrange(-Count) %>% 
  select(-n)

saveCSV(zipCount, desktop=T)
