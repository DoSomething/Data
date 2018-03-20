source('config/init.R')
source('config/mySQLConfig.R')
source('config/customFunctions.R')
library(httr)
library(jsonlite)
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
  answers <- as.tibble(cbind(res$responses$hidden, res$responses$answers))
  return(answers)
}

allTypeForm <- getKeyList(tfkey)

webKey <- 'Bvcwvm'

web <-
  getOutput(webKey, tfkey) %>%
  setNames(c('northstar_id','campaign_id','campaign_run_id',
             'origin','nps','text')) %>%
  select(-campaign_id, -origin) %>%
  filter(!is.na(northstar_id)) %>%
  mutate(nps=as.double(nps))

getNPS(web$nps, 10)

nsids <- web$northstar_id
source <- "phoenix-next"

query <- glue_sql(
  "SELECT
    c.northstar_id,
    c.signup_id
  FROM quasar.users u
  LEFT JOIN quasar.campaign_activity c ON u.northstar_id = c.northstar_id
  WHERE c.northstar_id IN ({northstars*})
  AND u.source = {source}",
  northstars = nsids,
  source = source,
  .con = con
)

qres <- runQuery(q,'mysql')


