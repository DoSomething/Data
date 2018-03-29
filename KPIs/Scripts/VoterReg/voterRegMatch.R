#https://trello.com/c/zEizjWFf/1183-data-request-member-cross-referencing-with-run-for-something-list
source('config/init.R')
source('config/mySQLConfig.R')
library(stringr)

nrod <- 
  read_csv('Data/NROD_Candidates.csv') %>% 
  filter(!is.na(Last_Name)) %>% 
  mutate(
    First_Name = toupper(str_replace_all(First_Name, "[^[:alnum:]]", " ")),
    Last_Name = toupper(str_replace_all(Last_Name, "[^[:alnum:]]", " "))
  )

fn <- prepQueryObjects(nrod$First_Name)
ln <- prepQueryObjects(nrod$Last_Name)
em <- prepQueryObjects(nrod$Email)

q <- 
  paste0(
    "SELECT
      u.northstar_id,
      u.first_name,
      u.last_name,
      u.email 
    FROM quasar.users u
    WHERE upper(u.email) in ", em
  )

qres <- runQuery(q, which='mysql')

match <- 
  nrod %>% 
  inner_join(qres, by=c('Email'='email'))

saveCSV(match, desktop=T)
