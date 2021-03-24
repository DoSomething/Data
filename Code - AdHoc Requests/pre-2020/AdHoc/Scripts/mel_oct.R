source('config/init.R')

rs <- 
  read_csv('Data/rideandseek2017_all_20171103_122813.csv') %>% 
  select(phone_number) %>% 
  setNames('mobile') %>% 
  mutate(mobile = as.character(mobile))

meloct <-
  read_csv('Data/events_october.csv') %>% 
  setNames(c('event_id','mobile','northstar_id','timestamp')) %>% 
  filter(timestamp <= '2017-10-31') 

countOct <- length(unique(meloct$northstar_id))

rs %>% 
  filter(
    !cleanPhone(mobile) %in% cleanPhone(meloct$mobile) & 
      !duplicated(mobile)
    )%>% 
  nrow() -> rssignups
