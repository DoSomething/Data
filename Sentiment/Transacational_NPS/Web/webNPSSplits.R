source('config/init.R')
source('config/mySQLConfig.R')
source('config/customFunctions.R')
library(httr)
library(jsonlite)
library(glue)
tfkey <- Sys.getenv('TYPEFORM_KEY')

getKeyList <- function(key) {
  allForms <- paste0('https://api.typeform.com/v1/forms?key=',key)
  res <- GET(url = allForms)
  json <- httr::content(res, as = "text")
  allTypeForms <- fromJSON(json)
  return(allTypeForms)
}

getResults <- function(surveyKey, tfkey) {
  npsfeedback <- paste0('https://api.typeform.com/v1/form/',surveyKey,'?key=',tfkey)
  res <- GET(url = npsfeedback)
  json <- content(res, as = "text")
  feedbackResults <- fromJSON(json)
  return(feedbackResults)
}

getOutput <- function(surveyKey, tfkey) {
  res <- getResults(surveyKey, tfkey)
  questions <- as.tibble(res$questions)
  answers <- as.tibble(cbind(res$responses$hidden, res$responses$answers,res$responses$metadata$date_submit))
  return(answers)
}

allTypeForm <- getKeyList(tfkey)

webKey <- 'Bvcwvm'

web <-
  getOutput(webKey, tfkey) %>%
  setNames(c('northstar_id','campaign_id','campaign_run_id',
             'origin','nps','text','response_ts')) %>%
  select(-campaign_id, -origin) %>%
  filter(!is.na(northstar_id)) %>%
  mutate(
    nps=as.double(nps),
    response_ts = as.POSIXct(response_ts, format='%Y-%m-%d %H:%M:%S')
    )

getNPS(web$nps, 10)

nsids <- web$northstar_id

query <- glue_sql(
  "SELECT
    u.northstar_id,
    count(*) AS campaigns
  FROM quasar.users u
  LEFT JOIN quasar.campaign_activity c ON u.northstar_id = c.northstar_id
  WHERE u.northstar_id IN ({northstars*})
  GROUP BY u.northstar_id",
  northstars = nsids,
  .con = con
)

qres <-
  runQuery(query,'mysql') %>%
  mutate(
    any_signup = case_when(
      campaigns > 0 ~ T,
      TRUE ~ F
    )
  )

query <-
  glue_sql(
    "SELECT
  		u.northstar_id,
      u.created_at,
      CASE WHEN source='niche' THEN 1 ELSE 0 END AS niche,
      CASE WHEN (u.customer_io_subscription_status IS NULL OR u.customer_io_subscription_status = 'subscribed')
        AND (u.email IS NULL OR LENGTH(u.email) = 0) AND u.mobile IS NOT NULL AND u.sms_status = 'active'
        THEN 1 ELSE 0 END AS sms_only,
      CASE WHEN count(DISTINCT c.signup_id) > 0 THEN 1 ELSE 0 END AS has_signup
    FROM quasar.users u
    LEFT JOIN quasar.campaign_activity c ON u.northstar_id=c.northstar_id
    WHERE u.sms_status='active' OR u.customer_io_subscription_status='subscribed'
    GROUP BY u.northstar_id"
  )

sign <- runQuery(query,'mysql')

signprep <-
  sign %>%
  mutate(
    created_at = as.POSIXct(created_at, format='%Y-%m-%d %H:%M:%S'),
    group = case_when(niche==1 ~ 'niche',
                      sms_only==1 ~ 'sms_only',
                      TRUE ~ 'all_others')
  )
samp <- signprep %>% sample_n(10000)
ggplot(samp, aes(x=as.factor(has_signup), y=created_at)) +
  geom_violin(position='dodge', aes(fill=as.factor(has_signup))) +
  coord_flip() +
  facet_grid(group~.) +
  labs(x='has_signup') +
  theme(legend.position="none")
