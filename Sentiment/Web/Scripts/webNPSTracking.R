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
    responseDist = 1/sqrt(as.numeric(Sys.Date() - as.Date(response_ts) + 1))
  )

scores <- numeric()
for (i in 1:1000) {
  samp <-
    web %>%
    sample_n(nrow(.), weight = responseDist, replace = T)
  score <- getNPS(samp$nps,10)
  scores <- c(scores, score)
}

out <- tibble(as_of_date = today(), NPS = round(mean(scores)))

channel <- pgConnect()
dbWriteTable(channel,c("public","web_nps"), out, row.names=F, append=T)
grant <- "grant select on web_nps to public;"
dbGetQuery(channel, grant)
