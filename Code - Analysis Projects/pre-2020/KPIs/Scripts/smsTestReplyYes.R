sms <- read_csv('Data/smsTestReplyYes.csv')
pg <- pgConnect()
library(glue)

q <- glue_sql(
  "SELECT
    u.northstar_id,
    u.created_at
  FROM public.users u
  WHERE u.northstar_id IN ({nsids*})",
  nsids = sms$userId,
  .con = pg
  )

res <- runQuery(q, 'pg')

out <-
  sms %>%
  left_join(res, by=c('userId' = 'northstar_id')) %>%
  select(
    id = userId,
    created_at
    ) %>%
  mutate(
    created_at=as.integer(created_at)
  ) %>%
  filter(!duplicated(id))

saveCSV(out)
