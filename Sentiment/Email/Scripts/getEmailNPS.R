suppressMessages(source('config/init.R'))
source('config/customFunctions.R')
# library(rtypeform)
#
# webKey <- 'ZZ0dsO'
# tfKey <- Sys.getenv('TYPEFORM_KEY')
#
# typeforms = get_typeforms(tfKey)
# quest <- get_questionnaire(webKey, tfKey, order_by = "date_submit_desc")
#
# email <-
#   quest$completed %>%
#   select(northstar_id=id, nps=opinionscale_D4ZaUkOuZ1P7, date_submit) %>%
#   mutate(nps=as.numeric(nps))

email <-
  read_csv('Email/Data/Email Q2.csv') %>%
  setNames(c('id','nps','reason','nsid','start_datetime',
             'complete_datetime','network_id'))

getNPS(email$nps,10)
ggplot(email, aes(x=as.factor(nps))) +
  geom_histogram(stat='count') +
  labs(title='Score Distribution via Survey',x='NPS') +
  theme(plot.title=element_text(hjust=.5))

q <- "
select
  e.customer_id,
  e.href
from cio.email_event e
where template_id in (1612,1583)
and event_type='email_clicked'"

qres <- runQuery(q)

#TODO: Why are there no prefilled answer values here?
matched <-
  qres %>%
  filter(customer_id %in% email$nsid)

parse <-
  qres %>%
  separate(href, c('a','b'), sep = '=') %>%
  separate(b, c('score','c'), sep='&') %>%
  mutate(score=as.double(score)) %>%
  filter(!is.na(score))

getNPS(parse$score, 10)
ggplot(parse, aes(x=as.factor(score))) +
  geom_histogram(stat='count') +
  ggtitle('Score Distribution via Click') +
  theme(plot.title=element_text(hjust=.5))

dupes <-
  parse %>%
  filter(duplicated(customer_id) | duplicated(customer_id, fromLast=T))

ggplot(dupes, aes(x=customer_id, y=as.factor(score), fill=customer_id)) +
  geom_bar(stat='identity', position=position_dodge(width=1), width=.8) +
  theme(legend.position = 'none', plot.title = element_text(hjust=.5)) +
  ggtitle('Scores by respondents with multiple clicks')
