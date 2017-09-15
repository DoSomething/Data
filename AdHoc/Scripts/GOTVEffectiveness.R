source('config/init.R')
source('config/mySQLConfig.R')


# Create Analytical Set ---------------------------------------------------

badEmails <- c('blah', '@dosomething', 'test', '@example', 'bot', 'thing.org')

rtv <- 
  read_csv('Data/RTV_DoSomethingOrg_APPENDED_20170724.csv') %>% 
  mutate(
    RTV_BIRTHDATE = as.Date(gsub(' 0:00', '', RTV_BIRTHDATE), format='%m/%d/%y'),
    voted = as.factor(ifelse(is.na(E2016GVM), 'yes', 'no'))
  ) %>% 
  select(DWID, FIRSTNAME, LASTNAME, AGE, GENDER, 
         RACE, MAILADDRSTATE, PHONE, RTV_BIRTHDATE, voted)

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
  select(Phone, Party, Email.address)

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
      rename(
        'birthdate'='RTV_BIRTHDATE', 
        'last_name' = 'LASTNAME', 
        'first_name' = 'FIRSTNAME'),
    copy=T
  ) %>% 
  select(northstar_id) -> ns2

matched <- ns1 %>% bind_rows(ns2) %>% filter(!duplicated(northstar_id))

uid <- prepQueryObjects(matched$northstar_id)

q <- paste0(
  "SELECT 
    u.northstar_id,
    u.mobile as phone, 
    u.email, 
    u.birthdate, 
    u.first_name,
    u.last_name,
    i.campaign_node_id_title as campaign,
    i.campaign_type,
    i.campaign_action_type,
    i.campaign_cause_type,
    c.signup_id,
    CASE WHEN c.signup_created_at IS NULL THEN NULL
         WHEN c.signup_created_at < '2016-11-08' THEN 1 
         ELSE 0 END AS pre_election,
    c.post_id,
    c.quantity
  FROM quasar.users u 
  LEFT JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
  LEFT JOIN quasar.campaign_info i ON i.campaign_run_id = c.campaign_run_id
  WHERE u.northstar_id IN ", uid)

camp.query <- runQuery(q)

cam <- 
  camp.query %>% 
  mutate(
    post_id = ifelse(is.na(post_id), -1, post_id),
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
  ) %>% 
  group_by(northstar_id, signup_id) %>% 
  filter(
    post_id == max(post_id) & 
      !duplicated(paste0(northstar_id, signup_id, post_id))) %>% 
  group_by(northstar_id) %>% 
  mutate(
    n_campaigns = length(which(!is.na(signup_id))),
    n_campaigns_pre_election = length(which(!is.na(signup_id) & pre_election == 1)),
    n_reportbacks = length(which(post_id != -1)),
    did_campaign = max(campaign_type == 'campaign'),
    did_sms_game = max(campaign_type == 'sms_game')
  ) %>% 
  left_join(
    camp.query %>% 
      filter(!is.na(campaign_action_type)) %>% 
      mutate(campaign_action_type = gsub(' ', '.', campaign_action_type)) %>% 
      group_by(northstar_id, campaign_action_type) %>% 
      summarise(N = n()) %>% 
      spread(campaign_action_type, N)
  ) %>% 
  left_join(
    camp.query %>% 
      filter(!is.na(campaign_cause_type)) %>% 
      mutate(campaign_cause_type = gsub(' ', '.', campaign_cause_type)) %>% 
      group_by(northstar_id, campaign_cause_type) %>% 
      summarise(N = n()) %>% 
      spread(campaign_cause_type, N)
  ) %>% 
  filter(!duplicated(northstar_id)) %>% 
  select(-campaign, -campaign_type, -campaign_action_type, 
         -campaign_cause_type,-signup_id, -post_id, -quantity) %>% 
  group_by(first_name, last_name, birthdate) %>% 
  filter(n_campaigns==max(n_campaigns)) %>% 
  ungroup()

set <- 
  rtv %>% 
  left_join(cam %>% 
              filter(!is.na(email)) %>% 
              select(email, northstar_id), 
            by = c('Email.address' = 'email')) %>% 
  left_join(cam %>% 
              filter(!is.na(phone))%>% 
              select(phone, northstar_id), 
            by = c('PHONE' = 'phone')) %>% 
  left_join(cam %>% 
              filter(
                !is.na(last_name) & 
                  last_name != '' & 
                  !is.na(birthdate) & 
                  !is.na(first_name) &
                  !duplicated(paste0(last_name, first_name, birthdate))
              ) %>% 
              rename('RTV_BIRTHDATE'='birthdate', 
                     'LASTNAME' = 'last_name', 
                     'FIRSTNAME' = 'first_name') %>% 
              select(RTV_BIRTHDATE, LASTNAME, FIRSTNAME, northstar_id)
            ) %>% 
  mutate(
    northstar_id = ifelse(!is.na(northstar_id), northstar_id,
                          ifelse(!is.na(northstar_id.x), northstar_id.x,
                                 ifelse(!is.na(northstar_id.y), northstar_id.y, NA))),
    voted.n = voted,
    voted = ifelse(voted=='yes', 1, 0),
    DSMember = ifelse(!is.na(northstar_id), 1, 0)
  ) %>% 
  select(-northstar_id.x, -northstar_id.y) %>% 
  left_join(
    cam %>% 
      select(-first_name,-last_name,-birthdate,-email,-phone,-pre_election), 
    by='northstar_id')

# Analysis ----------------------------------------------------------------
candidates <- c()

for (i in 1:length(colnames(set))) {
  temp <- paste0("'", colnames(set)[i], "',")  
  candidates <- paste0(candidates, temp)  
}

candidates <- 
  c('AGE','GENDER','RACE','MAILADDRSTATE','Party','n_campaigns',
    'n_campaigns_pre_election','n_reportbacks','did_campaign',
    'did_sms_game','Donate.Something','Face.to.Face','Host.An.Event',
    'Improve.a.Space','Make.Something','Share.Something','Start.Something',
    'Take.a.Stand','Animals','Bullying','Disasters','Discrimination',
    'Education','Environment','Homelessness','Mental.Health','Physical.Health',
    'Poverty','Relationships','Sex','Violence')

set %<>%
  filter(!is.na(northstar_id))
