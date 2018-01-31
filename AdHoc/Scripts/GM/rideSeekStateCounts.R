source('config/init.R')

q <- "
SELECT DISTINCT 
  c.northstar_id,
  c.signup_created_at,
  u.addr_state,
  u.addr_zip,
  CASE WHEN mel.action_ts >= c.signup_created_at 
       THEN 0 ELSE 1 END AS new_member
FROM quasar.campaign_activity c
INNER JOIN quasar.users u ON c.northstar_id = u.northstar_id
LEFT JOIN 
    (SELECT 
      m.northstar_id,
      min(m.`timestamp`) AS action_ts
    FROM quasar.member_event_log m
    GROUP BY m.northstar_id) mel 
  ON mel.northstar_id = c.northstar_id
WHERE c.campaign_run_id in (7931,7702) 
"

qres <- runQuery(q, 'mysql')

stateCount <- 
  qres %>% 
  group_by(new_member, addr_state) %>% 
  summarise(
    Count = n()
  ) %>% 
  spread(new_member, Count, fill = 0) %>% 
  rename(Existing=`0`, New=`1`) %>% 
  arrange(-New)

saveCSV(stateCount, desktop=T)

q <- "
SELECT 
  c.northstar_id,
  c.signup_created_at,
  u.addr_state,
  u.addr_zip,
  CASE WHEN mel.action_ts >= c.signup_created_at 
    THEN 0 ELSE 1 END AS new_member
FROM quasar.campaign_activity c
INNER JOIN quasar.users u ON c.northstar_id = u.northstar_id
LEFT JOIN 
    (SELECT 
    m.northstar_id,
    min(m.`timestamp`) AS action_ts
    FROM quasar.member_event_log m
    WHERE m.action_type = 'sign-up'
    GROUP BY m.northstar_id) mel 
ON mel.northstar_id = c.northstar_id
WHERE c.campaign_run_id=7702
"

qres.ctd <- runQuery(q, 'mysql')