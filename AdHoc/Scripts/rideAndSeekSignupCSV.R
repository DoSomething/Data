rideSeekPosterPeople <- function() {
  
  source('config/init.R')
  
  unfo <-
    read_csv('Data/ride_seek_signups_unformatted.csv')
  
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
      tid = gsub("\\:","",stripStrings(tid, strings)),
      text = gsub("\\:","",stripStrings(text, strings)),
      signup_created_at_timestamp = stripStrings(signup_created_at_timestamp, strings),
      signup_created_at_timestamp = substr(signup_created_at_timestamp,3,nchar(signup_created_at_timestamp)),
      score = gsub("\\:","",stripStrings(score, strings)),
      mobile = cleanPhone(stripStrings(mobile, strings)),
      campaign_node_id = 7930,
      campaign_run_id = 7931
    ) 
  
  q <-
    paste0(
      "SELECT 
    u.northstar_id,
    COALESCE(NULLIF(i.mobile, ''), NULLIF(u.mobile, '')) as mobile
    FROM quasar.users u
    LEFT JOIN quasar.moco_profile_import i ON i.moco_id = u.moco_commons_profile_id
    WHERE COALESCE(NULLIF(i.mobile, ''), NULLIF(u.mobile, '')) IS NOT NULL"
    )
  
  qres <- runQuery(q, 'mysql')
  
  phoneLook <- 
    qres %>% 
    mutate(mobile = cleanPhone(mobile))
  
  fo %<>%
    left_join(phoneLook) %>% 
    filter(!is.na(northstar_id) & !duplicated(northstar_id)) %>% 
    select(northstar_id, signup_created_at_timestamp, campaign_node_id, campaign_run_id)
  
  rsPoster <- fo$northstar_id
  
  saveCSV(fo, desktop = T)
  
  return(nsids)
}
