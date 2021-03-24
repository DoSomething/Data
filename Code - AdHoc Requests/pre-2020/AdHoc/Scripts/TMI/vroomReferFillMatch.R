source('config/init.R')

kwords <- 
  read_csv('Data/TMI_Mgage/All_Keywords.csv') %>% 
  rename(mobile = `mobile number`)
mast <- 
  read_csv('Data/TMI_Mgage/Week 57 Master List - All.csv') %>% 
  filter(is.na(Referrer)) %>% 
  rename(mobile = `Mobile Number`) %>%
  select(mobile)

## CAN WE DO NINO WITH ACCENT MARK?

keywordList <- c('CHILD','BFF','HIJO','OK','READY','LISTO','BABY',
                 'BEBE','DOOR','PUERTA','BRAIN','PAL','JOIN','NINO','KID')

kwordsOfInterest <- 
  kwords %>% 
  filter(toupper(message) %in% keywordList) %>% 
  inner_join(mast)

totals <- 
  kwordsOfInterest %>% 
  count(toupper(message))
