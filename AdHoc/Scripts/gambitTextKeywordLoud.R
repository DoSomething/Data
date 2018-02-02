#https://trello.com/c/OUfjyD6T/1230-data-request-text-in-loud

unfo <-
  read_csv('Data/textLoudUnformatted.csv')

tid <- unfo %>% filter(grepl('_id', X)) %>% select(X)
text <- unfo %>% filter(grepl('text', X)) %>% select(X)
createdAt <- unfo %>% filter(grepl('createdAt', X)) %>% select(X)
score <- unfo %>% filter(grepl('score', X)) %>% select(X)
mobile <- unfo %>% filter(grepl('plat', X2)) %>% select(X2)

strings <- c('platformUserId','ObjectId','score','ISODate','text','_id','createdAt','"',",","\\(","\\)")

stripStrings <- function(x, strings) {
  # x <- gsub("[^[:alnum:] ]", "", x)
  x <- gsub(paste(strings, collapse = "|"), "", x)
  x <- trimws(x)
  return(x)
}

fo <- 
  bind_cols(tid, text, createdAt, score, mobile) %>% 
  setNames(c('tid','text','signup_created_at_timestamp','score','mobile')) %>% 
  mutate(
    tid = trimws(gsub("\\:","",stripStrings(tid, strings))),
    text = trimws(gsub("\\:","",stripStrings(text, strings))),
    signup_created_at_timestamp = stripStrings(signup_created_at_timestamp, strings),
    signup_created_at_timestamp = substr(signup_created_at_timestamp,3,nchar(signup_created_at_timestamp)),
    signup_created_at_timestamp = paste0(substr(signup_created_at_timestamp, 1, 10),
                                         ' ', substr(signup_created_at_timestamp, 12, 19)),
    signup_created_at_timestamp = as.Date(signup_created_at_timestamp, '%Y-%m-%d %H:%M:%S'),
    score = trimws(gsub("\\:","",stripStrings(score, strings))),
    mobile = cleanPhone(stripStrings(mobile, strings))
  ) %>% 
  group_by(signup_created_at_timestamp) %>% 
  summarise(
    Count = n()
  )

