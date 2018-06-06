source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

tmi <- 
  read_csv('Data/tmiKeywordsTextIns.csv')

q <- 
  glue_sql(
    "SELECT
      u.northstar_id,
      u.first_name,
      u.last_name,
      u.mobile,
      u.email
    FROM public.users u 
    WHERE u.northstar_id IN ({nsids*})",
    nsids = tmi$userId,
    .con = pg
  )

qres <- runQuery(q, 'pg')

out <- 
  tmi %>% 
  left_join(qres, by = c('userId' = 'northstar_id'))
