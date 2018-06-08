source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

q <- 
  "SELECT
    u.id,
    u.sms_status,
    u.updated_at
  FROM northstar.users u
  INNER JOIN
    (SELECT 
        count(DISTINCT b.sms_status), 
        b.id 
    FROM northstar.users b
    GROUP BY b.id
    HAVING count(DISTINCT b.sms_status) >1) upt ON upt.id = u.id"

qres <- 
  runQuery(q,'pg')

samp <- 
  qres %>% 
  group_by(id) %>% 
  filter(max(updated_at) >= '2018-06-01') %>% 
  sample_frac(.2)

proc <- 
  qres %>% 
  group_by(id) %>% 
  filter(max(updated_at) >= '2018-06-01') %>% 
  ungroup() %>% 
  mutate(
    sms_status =
      case_when(
        is.na(sms_status) ~ '',
        sms_status %in% c('active','less') ~ 'active',
        TRUE ~ sms_status
        )
  ) %>% 
  arrange(id, desc(updated_at)) %>% 
  group_by(id) %>% 
  mutate(
    recordCounter = 1:n()
  ) %>% 
  filter(recordCounter <= 2)

p2 <- 
  proc %>% 
  mutate(
    prev_status = lead(sms_status)
  ) %>% 
  filter(recordCounter==1 & sms_status != prev_status) %>% 
  select(-recordCounter)

col <- 
  p2 %>% 
  ungroup() %>% 
  count(updated_at,sms_status,prev_status) %>% 
  arrange(-n)

saveCSV(col, desktop=T)
