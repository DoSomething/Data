source('config/init.R')
source('config/mySQLConfig.R')
library(dbplyr)

# q <- "
# SELECT
#   u.northstar_id,
#   u.email,
#   u.northstar_created_at_timestamp AS create_date,
#   u.last_accessed
# FROM quasar.users u
# INNER JOIN quasar.campaign_activity c ON u.northstar_id = c.northstar_id
# WHERE u.northstar_created_at_timestamp >= '2017-01-01'
# AND (u.moco_current_status = 'active' OR
#     u.customer_io_subscription_status = 'subscribed')
# AND u.country = 'US'
# AND u.email NOT like '%dosomething.org%'
# AND "
#
#
#
# qres <- runQuery(q,which = 'mysql')
yearAgo <-Sys.Date()-365

qres <-
  tbl(con, "users") %>%
  inner_join(
    tbl(con, "campaign_activity") %>%
      filter(signup_created_at >= yearAgo) %>%
      select(northstar_id, signup_created_at)
    ) %>%
  mutate(
    create_date=northstar_created_at_timestamp,
    source=northstar_id_source_name
    ) %>%
  filter(
    (moco_current_status == 'active' |
       customer_io_subscription_status == 'subscribed') &
      country=='US'
    ) %>%
  select(northstar_id, email, create_date, birthdate,
         source, mobile, signup_created_at) %>%
  collect() %>%
  filter(
    !grepl('dosomething.org', email)
  )
