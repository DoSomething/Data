#https://trello.com/c/G0rMrSRR/1248-data-request-sms-week-of-action-march-for-our-lives-rivescript-query
source('config/init.R')

unfo <-
  read_csv('Data/m4ol_responses.csv')

text <- unfo %>% filter(grepl('text', X)) %>% select(X)
broadcastId <- unfo %>% filter(grepl('broadcastId', X)) %>% select(X)
topic <- unfo %>% filter(grepl('topic', X2)) %>% select(X2)
nsid <- unfo %>% filter(grepl('user', X2)) %>% select(X2)

strings <- c('userId','topic','text','text','broadcastId','m4ol','"',",","\\(","\\)")

stripStrings <- function(x, strings) {
  x <- gsub("[^[:alnum:] ]", "", x)
  x <- gsub(paste(strings, collapse = "|"), "", x)
  x <- trimws(x)
  return(x)
}

fo <- 
  bind_cols(nsid, text, broadcastId, topic) %>% 
  setNames(c('nsid','text','broadcastId','topic')) %>% 
  mutate(
    nsid = trimws(gsub("\\:","",stripStrings(nsid, strings))),
    text = trimws(gsub("\\:","",stripStrings(text, strings))),
    broadcastId = trimws(gsub("\\:","",stripStrings(broadcastId, strings))),
    topic = stripStrings(topic, strings)
  ) %>% 
  filter(!duplicated(nsid))

