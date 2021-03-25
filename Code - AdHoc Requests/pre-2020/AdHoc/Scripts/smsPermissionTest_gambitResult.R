source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

testResults <-  
  read_csv('Data/smsPermissionTest.csv') %>% 
  filter(
    broadcastId %in% c('3eidTdgN1eWSqe4oA6M8kW','703YdcvbosWuSKA08cgq4m') & 
      (!match %in% c('d','stop','[*]'))
    ) %>% 
  group_by(broadcastId,match) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n)) %>% 
  arrange(broadcastId,-p)

optOuts <- 
  read_csv('Data/smsPermissionTest.csv') %>% 
  filter(match %in% c('d','stop','[*]'))

q <- 
  glue_sql(
    "SELECT
      u.northstar_id,
      u.email
    FROM public.users u
    WHERE u.northstar_id IN ({nsids*})",
    .con=pg,
    nsids=optOuts$userId
  )

qres <- runQuery(q)
