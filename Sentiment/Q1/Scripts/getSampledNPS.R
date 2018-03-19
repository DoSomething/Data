source('config/init.R')
source('config/customFunctions.R')
source('config/mySQLConfig.R')
library(httr)
library(jsonlite)
tfkey <- Sys.getenv('TYPEFORM_KEY')

getPopBreakdown <- function() {
  q <- paste0("
  SELECT
            count(*) AS total_active,
            sum(CASE WHEN (((u.customer_io_subscription_status IS NULL) OR u.customer_io_subscription_status = 'subscribed'))
            AND ((u.email IS NULL OR LENGTH(u.email ) = 0 )) AND ((u.mobile IS NOT NULL)) AND (u.sms_status = 'active')
            THEN 1 ELSE 0 END) AS sms_only,
            sum(CASE WHEN source='niche' THEN 1 ELSE 0 END) AS niche
            FROM quasar.users u
            WHERE (u.sms_status = 'active' OR
            u.customer_io_subscription_status = 'subscribed')
            AND u.email NOT like '%dosomething.org%'"
  )

  qres <- runQuery(q, which = 'mysql')

  pop <-
    qres %>%
    mutate(
      other = total_active-sms_only-niche
    ) %>%
    melt() %>%
    filter(variable!='total_active') %>%
    setNames(c('group','n')) %>%
    mutate(p=n/sum(n))

  return(pop)
}

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

smsq1Key <- 'ENmagA'
regularq1Key <- 'AbLc7y'
nicheq1Key <- 'gD7ObP'

getOutput <- function(surveyKey, tfkey) {
  res <- getResults(surveyKey, tfkey)
  questions <- as.tibble(res$questions)
  answers <- as.tibble(cbind(res$responses$hidden, res$responses$answers))
  answers %<>%
    filter(!is.na(id)) %>%
    select(id, starts_with('opinion')) %>%
    setNames(c('id','nps')) %>%
    mutate(nps=as.numeric(nps))
  return(answers)
}

smsOut <- getOutput(smsq1Key, tfkey) %>% mutate(group='sms_only')
regOut <- getOutput(regularq1Key, tfkey) %>% mutate(group='other')
nicheOut <- getOutput(nicheq1Key, tfkey) %>% mutate(group='niche')

pop <- getPopBreakdown()

all <-
  bind_rows(smsOut, regOut, nicheOut) %>%
  left_join(pop)

nps.SMS <- getNPS(as.numeric(smsOut$nps),10)
nps.Reg <- getNPS(as.numeric(regOut$nps),10)
nps.Niche <- getNPS(as.numeric(nicheOut$nps),10)


scores <- numeric()
for (i in 1:10000) {
  samp <-
    all %>%
    sample_n(nrow(.), weight = p, replace = T)
  score <- getNPS(samp$nps,10)
  scores <- c(scores, score)
}
scores <- tibble(score = scores)
mean(scores$score)
ggplot(scores, aes(x=score)) + geom_density()
