# https://trello.com/c/D9zdprV1/1316-gtm-typeform-mid-year-check-in

source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

gtm <- read_csv('Data/Grab the Mic July.csv')

q <- 
  glue_sql(
    "SELECT 
      rbs.northstar_id AS id,
      sum(rbs.rbs) AS reportbacks
    FROM
        (SELECT
          c.northstar_id,
          sum(c.reportback_volume) as rbs
        FROM public.campaign_activity c
        WHERE c.northstar_id IN ({nsids*})
        AND c.campaign_run_id = '8022'
        GROUP BY c.northstar_id
      UNION
        SELECT 
          e.northstar_id,
          count(*) AS rbs
        FROM public.phoenix_events e
        WHERE e.event_name IN ('share action completed','facebook share posted')
        AND e.campaign_id = '8017'
        AND e.northstar_id IN ({nsids*})
        GROUP BY e.northstar_id) rbs
    GROUP BY rbs.northstar_id
    ",
    .con=pg,
    nsids=gtm$id
  )

qres <- runQuery(q)

gtmSurveyReponses_reportbacks <- 
  gtm %>% 
  left_join(qres) %>% 
  mutate(
    reportbacks = case_when(
      is.na(reportbacks) ~ 0,
      TRUE ~ reportbacks
    )
  )

saveCSV(gtmSurveyReponses_reportbacks)
