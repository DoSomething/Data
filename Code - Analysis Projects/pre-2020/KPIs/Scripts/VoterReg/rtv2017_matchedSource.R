source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

rtv <- 
  read_csv('../KPIs/Data/RockTheVote/rock_the_vote_2017.csv') %>% 
  rename(
    created_at=`Started registration`,
    email=`Email address`,
    mobile=Phone
    ) %>% 
  filter(
    Status=='Complete' &
    created_at>='2017-09-26' &
    created_at<'2017-10-02' 
    ) %>% 
  mutate(
    mobile=str_replace_all(mobile, "[^[0-9]]", ""),
    email=toupper(str_replace_all(email, "[^[:alnum:]]", " "))
  )

q <- 
  glue_sql(
    "SELECT 
      u.northstar_id,
      u.source,
      u.email,
      u.mobile
    FROM public.users u
    WHERE 
      UPPER(u.email) IN ({email*}) OR
      u.mobile IN ({mobile*})
    ",
    .con = pg,
    email = rtv$email,
    mobile = rtv$mobile
  )

qres <- runQuery(q)

join <- 
  qres %>% 
  mutate(
    source = 
      case_when(
        grepl('phoenix',source) | source=='niche' ~ 'web',
        is.na(source) & !is.na(mobile) ~ 'sms',
        TRUE ~ source
      )
  )

pct <- pctChange(nrow(join),nrow(rtv))

sourceEst <- 
  join %>% 
  count(source) %>% 
  mutate(
    est = applyPctChange(n,pct)
  )
