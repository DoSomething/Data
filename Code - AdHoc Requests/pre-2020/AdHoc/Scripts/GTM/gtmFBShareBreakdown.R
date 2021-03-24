# https://trello.com/c/LgSDsmM7/1288-data-request-grab-the-mic-member-engagement

source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

gtmFBShares <- 
  runQuery('Scripts/gtmFBComparison.sql') %>% 
  mutate(url=ifelse(url=='grab-mic','grab-mic/',url)) %>% 
  group_by(month,url) %>% 
  summarise(
    count=sum(count)
  ) %>% 
  spread(month, count)

saveCSV(gtmFBShares)
