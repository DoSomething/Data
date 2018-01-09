source('config/init.R')
rerun = F
# Functions ---------------------------------------------------------------

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
    grepl("Don't", x) ~ .25,
    grepl('Comment', x) ~ .5,
    grepl('Ask', x) ~ .75,
    grepl('stop', tolower(x)) ~ 1,
    is.na(x) ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
  return(y)
}


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
  filter((response_status!='Started' | is.na(response_status)) & 
           !duplicated(survey_id))

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

lameVars <- c('distract_none', 'X1', 'ip_address', 'country',
              'language', 'device_survey', 'operating_system',
              'response_status','time_to_complete','network_id')

dat <- 
  surv %>% 
  mutate_at(vars(starts_with('distract')), recodeOneValNA) %>% 
  mutate_at(vars(starts_with('danger')), function(x) as.numeric(substr(x, 1, 1))) %>% 
  mutate_at(vars(starts_with('seatbelt_')), recodeSeatbelt) %>% 
  mutate_at(vars(starts_with('intervene_')), recodeIntervention) %>% 
  mutate(
    distraction_prone = rowMeans(.[grep("distract_", names(.))], na.rm = TRUE),
    considers_dangers = scalerange(rowMeans(.[grep("danger", names(.))], na.rm = TRUE)),
    # seatbelt_usage = rowMeans(.[grep("seatbelt_", names(.))], na.rm = TRUE),
    willing_intervene = rowMeans(.[grep("intervene_", names(.))], na.rm = TRUE),
    survey_submit = as.Date(paste0(date_submitted, ':00'), '%m/%d/%y %H:%M:%S'),
    signup_date = as.Date(signup_date, '%Y-%m-%d %H:%M:%S'),
    time_to_survey = survey_submit - signup_date,
    gender = case_when(
      gender == 'Female' ~ 'Female',
      gender == 'Male' ~ 'Male',
      TRUE ~ 'Other'
    ),
    age = case_when(
      age == 'Older than 25' ~ 26,
      age == 'Younger than 15' ~ 14,
      TRUE ~ as.numeric(age)
    )
  ) %>% 
  filter(!is.na(driving_freq)) %>% 
  select(-one_of(lameVars), -starts_with('distract_'))

save(dat, file = 'Data/rideSeekSurveyAnalyticalSet.Rdata')
