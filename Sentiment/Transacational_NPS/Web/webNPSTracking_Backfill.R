source('config/init.R')
source('config/pgConnect.R')
source('config/customFunctions.R')
library(httr)
library(jsonlite)
library(glue)

allTypeForm <- getKeyList(Sys.getenv('TYPEFORM_KEY'))

webKey <- 'Bvcwvm'

web <-
  getOutput(webKey, Sys.getenv('TYPEFORM_KEY')) %>%
  setNames(c('northstar_id','campaign_id','campaign_run_id',
             'origin','nps','text','response_ts')) %>%
  select(-campaign_id, -origin) %>%
  filter(!is.na(northstar_id)) %>%
  mutate(
    nps=as.double(nps),
    response_ts = as.POSIXct(response_ts, format='%Y-%m-%d %H:%M:%S'),
    responseDist = 1/sqrt(as.numeric(Sys.Date() - as.Date(response_ts) + 1)),
    response_ts = as.Date(response_ts)
  )

dateSeq <-
  seq.Date(
    min(as.Date(web$response_ts)),
    max(as.Date(web$response_ts)), '1 days'
    )

outTable <- tibble()
for (i in 1:length(dateSeq)) {
  thisRun <-
    web %>%
    filter(response_ts <= dateSeq[i]) %>%
    mutate(
      responseDist = 1/sqrt(as.numeric(dateSeq[i] - as.Date(response_ts) + 1))
    )
  print(max(thisRun$response_ts))
  print(nrow(thisRun))
  print(mean(thisRun$responseDist))

  scores <- numeric()
  for (i in 1:1000) {
    samp <-
      thisRun %>%
      sample_n(nrow(.), weight = responseDist, replace = T)
    score <- getNPS(samp$nps,10)
    scores <- c(scores, score)
  }

  thisScore <-
    tibble(
      as_of_date = max(thisRun$response_ts),
      NPS = round(mean(scores))
      )

  outTable %<>%
    bind_rows(thisScore)

}

channel <- pgConnect()
dbWriteTable(channel,c("public","web_nps"), outTable, row.names=F)

