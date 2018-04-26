source('config/init.R')
source('config/customFunctions.R')
source('config/mySQLConfig.R')
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

allTypeForm <- getKeyList(tfkey)

gtmMarch <- 'cCoZe3'

getOutput <- function(surveyKey, tfkey) {

  res <- getResults(surveyKey, tfkey)
  questions <- as.tibble(res$questions)
  answers <- as.tibble(
    cbind(res$responses$metadata, res$responses$hidden, res$responses$answers)
    )
  answers %<>%
    filter(!is.na(id))
  return(answers)
}

gtm <- getOutput(gtmMarch, tfkey)
