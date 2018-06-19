suppressMessages(source('config/init.R'))
source('config/customFunctions.R')
library(rtypeform)

webKey <- 'ZZ0dsO'
tfKey <- Sys.getenv('TYPEFORM_KEY')

typeforms = get_typeforms(tfKey)
quest <- get_questionnaire(webKey, tfKey, order_by = "date_submit_desc")

email <-
  quest$completed %>%
  select(northstar_id=id, nps=opinionscale_D4ZaUkOuZ1P7, date_submit) %>%
  mutate(nps=as.numeric(nps))

q <- "
select
  e.customer_id,
  e.href
from cio.email_event e
where e.template_id=1583 and event_type='email_clicked'"

qres <- runQuery(q)

parse <-
  qres %>%
  separate(href, c('a','b'), sep = '=') %>%
  separate(b, c('score','c'), sep='&')
