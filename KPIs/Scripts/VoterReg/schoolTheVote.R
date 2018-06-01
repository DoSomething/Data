source('config/init.R')
library(glue)
pg <- pgConnect()

q <-
  glue_sql(
    "SELECT
      c.northstar_id,
      u.source AS user_source,
      c.post_id AS id,
      c.post_created_at AS created_at,
      c.campaign_run_id,
      c.quantity
    FROM campaign_activity c
    LEFT JOIN public.users u ON c.northstar_id = u.northstar_id
    WHERE campaign_id = '822'
    AND c.signup_created_at>='2018-05-01'
    AND c.post_id IS NOT NULL
    AND c.post_status='accepted';",
    .con = pg
    )

qres <-
  runQuery(q,'pg')

stv <- tibble()
for (i in 1:nrow(qres)) {

  row <- qres[i,]
  temp <-
    tibble(
      nsid=rep(row$northstar_id,row$quantity),
      created_at = rep(row$created_at, row$quantity),
      user_source = rep(row$user_source, row$quantity),
      source = rep('school_the_vote', row$quantity),
      source_details = rep('school_the_vote', row$quantity),
      ds_vr_status = rep('register', row$quantity),
      file = rep('OnTheGround')
      )

  stv <-
    temp %>%
    bind_rows(stv)

}
