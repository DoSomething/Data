source('config/init.R')
source('config/mySQLConfig.R')

badEmails <- c('blah', '@dosomething', 'test', '@example', 'bot', 'thing.org')

rtv <- 
  read_csv('Data/RTV_DoSomethingOrg_APPENDED_20170724.csv') %>% 
  mutate(
    RTV_BIRTHDATE = as.Date(gsub(' 0:00', '', RTV_BIRTHDATE), format='%m/%d/%y')
  )

port <- 
  read_csv('Data/RTV_DoSomething_registrations.csv') %>% 
  setNames(make.names(names(.))) %>% 
  tbl_dt() %>% 
  mutate(
    badEmail = ifelse(grepl(paste(badEmails,collapse="|"), Email.address), T, F),
    Phone = cleanPhone(Phone)
  ) %>% 
  group_by(Phone) %>% 
  mutate(
    maxTS = max(as_datetime(Started.registration, tz = 'UTC'))
  ) %>% 
  ungroup(Phone) %>% 
  filter(
    as_datetime(Started.registration, tz = 'UTC') == maxTS & 
      !is.na(Phone) &
      !grepl(paste(badEmails,collapse="|"), Email.address)
  ) %>% 
  select(Phone, Email.address)

rtv <-
  rtv %>% 
  left_join(port, by = c('PHONE' = 'Phone'))

q <- "
SELECT 
u.northstar_id, 
u.mobile as phone, 
u.email, 
u.birthdate, 
u.first_name,
u.last_name
FROM quasar.users u
WHERE u.northstar_created_at_timestamp >= '2014-01-01'
"

qres <- runQuery(q)

users <- 
  qres %>% 
  mutate(
    last_name = toupper(gsub("[^[:alnum:] ]", "", last_name)),
    last_name =
      ifelse( (is.na(last_name) | last_name=='') & grepl(' ', last_name, ignore.case=T),
              str_split(last_name, ' ')[[1]][2], last_name),
    first_name = toupper(gsub("[^[:alnum:] ]", "", first_name)),
    first_name =       
      ifelse( (is.na(last_name) | last_name=='') & grepl(' ', last_name, ignore.case=T),
              str_split(first_name, ' ')[[1]][1], first_name),
    phone = cleanPhone(phone),
    birthdate = as.Date(birthdate, format = '%Y-%m-%d %H:%M:%S')
  )

users %>% 
  filter(
    (phone %in% rtv$PHONE & !is.na(phone)) |
      (email %in% rtv$Email.address & !is.na(email))
  ) %>% 
  select(northstar_id) -> ns1

users %>% 
  filter(
    !is.na(last_name) & 
      last_name != '' & 
      !is.na(birthdate) & 
      !is.na(first_name)
  ) %>% 
  inner_join(
    rtv %>% 
      select(RTV_BIRTHDATE, LASTNAME, FIRSTNAME) %>% 
      rename('birthdate'='RTV_BIRTHDATE', 'last_name' = 'LASTNAME', 'first_name' = 'FIRSTNAME'),
    copy=T
  ) %>% 
  select(northstar_id) -> ns2

matched <- ns1 %>% bind_rows(ns2) %>% filter(!duplicated(northstar_id))

uid <- prepQueryObjects(matched$northstar_id)

q <- paste0(
  "SELECT 
  c.northstar_id,
  c.campaign_node_id_title as campaign,
  c.campaign_type,
  c.campaign_action_type,
  c.campaign_cause_type,
  c.signup_id,
  c.post_id,
  c.quantity
  FROM quasar.users u 
  LEFT JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
  LEFT JOIN quasar.campaign_info i ON i.campaign_run_id = c.campaign_run_id
  WHERE u.northstar_id IN ", uid)

cam <- runQuery(q)
