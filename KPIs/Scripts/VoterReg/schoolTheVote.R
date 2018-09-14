source('config/init.R')
library(glue)

q <-
  glue_sql(
    "SELECT
      c.northstar_id,
      CASE WHEN u.source = 'niche' THEN 'niche'
           WHEN u.source = 'sms' THEN 'sms'
           ELSE 'web' END AS user_source,
      c.post_id AS id,
      c.post_created_at AS created_at,
      c.campaign_run_id::varchar,
      c.quantity
    FROM campaign_activity c
    LEFT JOIN public.users u ON c.northstar_id = u.northstar_id
    WHERE c.campaign_id IN ('822','8129','8119','8195','8202','8180','8208')
    AND c.signup_created_at>='2018-05-01'
    AND c.post_id IS NOT NULL
    AND (
      (c.campaign_id IN ('822','8129','8195','8202','8180','8208') AND c.post_status='accepted')
      OR (c.campaign_id IN ('8119') AND c.post_status <> 'rejected')
    )
    ;",
    .con = pg
    )

qres <-
  runQuery(q)

stv <- tibble()
for (i in 1:nrow(qres)) {

  row <- qres[i,]
  temp <-
    tibble(
      nsid=rep(row$northstar_id,row$quantity),
      created_at = rep(row$created_at, row$quantity),
      user_source = rep(row$user_source, row$quantity),
      source = rep('on_the_ground',row$quantity),
      source_details = rep(
        case_when(
          row$campaign_run_id %in% c('8103','8171') ~ 'school_the_vote',
          row$campaign_run_id %in% c('8130','8151') ~ 'red_white_booth',
          row$campaign_run_id=='8120' ~ 'community_partner',
          TRUE ~ ''
        ),
        row$quantity
      ),
      details = rep(
        case_when(
          row$campaign_run_id=='8103' ~ 'school_the_vote',
          row$campaign_run_id %in% c('8130','8151') ~ 'red_white_booth',
          row$campaign_run_id=='8120' ~ 'community_partner',
          TRUE ~ ''
        ),
        row$quantity
      ),
      ds_vr_status = rep('register', row$quantity),
      campaign_run_id = rep(row$campaign_run_id, row$quantity),
      month = month(created_at),
      week = case_when(
        created_at < '2018-02-06' ~ as.character('2018-01-26'),
        TRUE ~
          cut(
            as.Date(created_at),
            breaks=
              seq.Date(as.Date('2018-02-06'),as.Date('2019-01-01'),by = '7 days')
          ) %>% as.character()
      ),
      file = 'OnTheGround',
      campaign_id = case_when(
        campaign_run_id == '8120' ~ '8119',
        campaign_run_id == '8130' ~ '8129',
        campaign_run_id == '8103' ~ '822',
        TRUE ~ ''
        ),
      reportback = T
      )

  stv <-
    temp %>%
    bind_rows(stv)

}
