setwd('~/Data/Sentiment/')
suppressMessages(source('config/init.R'))
suppressMessages(source('config/pgConnect.R'))
source('config/customFunctions.R')
library(rtypeform)

webKey <- 'Bvcwvm'
tfKey <- Sys.getenv('TYPEFORM_KEY')

typeforms = get_typeforms(tfKey)
quest <- get_questionnaire(webKey, tfKey, completed = T, order_by = "date_submit_desc")

web <-
  quest$completed %>%
  select(northstar_id, opinionscale_L4EinsGMPo38, date_submit) %>%
  setNames(c('northstar_id','nps','date_submit')) %>%
  filter(!is.na(northstar_id)) %>%
  mutate(
    nps=as.double(nps),
    date_submit = as.POSIXct(date_submit, format='%Y-%m-%d %H:%M:%S',tz = 'UTC'),
    responseDist = 1/sqrt(as.numeric(Sys.Date() - as.Date(date_submit) + 1))
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
grant <- "grant select on public.web_nps to looker;"
dbGetQuery(channel, grant)
grant <- "grant select on web_nps to jjensen;"
dbGetQuery(channel, grant)
grant <- "grant select on web_nps to jli;"
dbGetQuery(channel, grant)
