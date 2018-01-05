source('config/init.R')

# Data Cleaning -----------------------------------------------------------

surv <-
  read_csv('Data/rideSeekSurveyResults.csv') %>% 
  mutate(
    mobile = cleanPhone(mobile)
  )

needNsid <- 
  surv %>% 
  filter(is.na(nsid) & !is.na(mobile)) %>% 
  select(mobile) %>% unlist()

q <-
  paste0(
    "SELECT 
    u.northstar_id,
    COALESCE(NULLIF(i.mobile, ''), NULLIF(u.mobile, '')) as mobile
    FROM quasar.users u
    LEFT JOIN quasar.moco_profile_import i ON i.moco_id = u.moco_commons_profile_id
    WHERE COALESCE(NULLIF(i.mobile, ''), NULLIF(u.mobile, '')) IS NOT NULL"
  )

qres <- runQuery(q, 'mysql')

phoneLook <- 
  qres %>% 
  mutate(mobile = cleanPhone(mobile)) %>% 
  filter(mobile %in% needNsid)

surv %<>%
  left_join(phoneLook) %>% 
  mutate(
    nsid = ifelse(is.na(nsid), northstar_id, nsid),
    northstar_id=NULL
    ) %>% 
  filter(response_status!='Started' & !duplicated(survey_id))

nsids <- prepQueryObjects(surv$nsid)

q <- paste0(
  "
SELECT 
  u.northstar_id,
  u.birthdate,
  u.created_at AS registration_date,
  MIN(c.signup_created_at) as signup_date,
  MAX(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) AS reportback
FROM quasar.users u
INNER JOIN campaign_activity c ON c.northstar_id = u.northstar_id
WHERE c.campaign_run_id = 7931
AND u.northstar_id in", nsids,
"GROUP BY u.northstar_id"
  )

camp_attributes <- runQuery(q, 'mysql')

surv %<>%
  left_join(camp_attributes, c('nsid' = 'northstar_id'))

recodeSeatbelt <- function(x) {
  y <- case_when(
    x %in% c('Never','Almost Never') ~ 0,
    x %in% c('Less than half the time') ~ .25,
    x %in% c('Most of the time','At least half of the time') ~ .75,
    x %in% c('Every time') ~ 1,
    is.na(x) ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
  return(y)
}

recodeOneValNA <- function(x) {
  y <- ifelse(is.na(x), 0, 1)
  return(y)
}

recodeIntervention <- function(x) {
  y <- case_when(
    grepl('nothing', x) ~ 0,
    grepl('Comment', x) ~ .25,
    grepl("Don't", x) ~ .5,
    grepl('Ask', x) ~ .75,
    grepl('stop', tolower(x)) ~ 1,
    is.na(x) ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
  return(y)
}

dat <- 
  surv %>% 
  mutate_at(vars(starts_with('distract')), recodeOneValNA) %>% 
  mutate_at(vars(starts_with('danger')), function(x) as.numeric(substr(x, 1, 1))) %>% 
  mutate_at(vars(starts_with('seatbelt_')), recodeSeatbelt) %>% 
  mutate_at(vars(starts_with('intervene_')), recodeIntervention) %>% 
  select(-distract_none) %>% 
  mutate(
    distraction_prone = rowMeans(.[grep("distract_", names(.))], na.rm = TRUE),
    considers_dangers = rowMeans(.[grep("danger", names(.))], na.rm = TRUE),
    # seatbelt_usage = rowMeans(.[grep("seatbelt_", names(.))], na.rm = TRUE),
    willing_intervene = rowMeans(.[grep("intervene_", names(.))], na.rm = TRUE)
  )
