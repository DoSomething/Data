source('config/init.R')
library(glue)

smsout <-
  read_csv('Data/smsVoterRegTestOngoing.csv')

smsIn <-
  vr %>%
  filter(details=='sms_tests') %>%
  select(id, nsid, ds_vr_status)

combine <-
  smsout %>%
  left_join(smsIn, by = c('userId' = 'nsid')) %>%
  filter(ds_vr_status %in% c('uncertain') | is.na(ds_vr_status)) %>%
  select(userId, broadcastId, ds_vr_status)

pg <- pgConnect()
q <- glue_sql(
  "SELECT
    u.northstar_id AS \"userId\",
    u.mobile,
    u.email as northstar_email
  FROM public.users u
  WHERE u.northstar_id IN ({nsids*})",
  nsids=combine$userId,
  .con=pg
)

qres <- runQuery(q, 'pg')

export <-
  combine %>%
  left_join(qres)

saveCSV(export)
