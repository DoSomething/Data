# https://trello.com/c/XKsJy425/1516-data-request-template
source('config/init.R')
library(glue)
pg <- pgConnect()

utw <- read_csv('~/Downloads/responses.csv')

res <- 
  glue_sql(
    "SELECT
      s.northstar_id AS id,
      MAX(
        CASE 
          WHEN s.id IS NOT NULL THEN 1
          ELSE 0 END
        ) AS signed_up,
      MAX(
        CASE 
          WHEN r.post_id IS NOT NULL THEN 1 
          ELSE 0 END
        ) AS reported_back
    FROM signups s
    LEFT JOIN reportbacks r ON r.signup_id = s.id
    WHERE s.campaign_id='9003'
      AND s.northstar_id IN ({nsids*})
    GROUP BY s.northstar_id",
    .con = pg,
    nsids = utw$id
  ) %>% 
  runQuery(.)

utw %<>%
  left_join(res) %>% 
  mutate(
    signed_up = replace_na(signed_up, 0),
    reported_back = replace_na(reported_back, 0)
  )
  
saveCSV(utw)
