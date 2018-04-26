source('config/init.R')
source('config/pgConnect.R')
pg <- pgConnect()
require(RMySQL)

con <-
  dbConnect(
    MySQL(),
    user='shasan',
    host='rogue-db-prod.c9ajz690mens.us-east-1.rds.amazonaws.com',
    password='sFOhX7pfYgRny4LnT7dJ',
    dbname='rogue_prod'
  )

q <- "select s.id as signup_id from rogue_prod.signups s"
rog <- tbl_df(dbGetQuery(con, q))

q <- "select s.id as signup_id from rogue.signups s"
pg <- runQuery(q, 'pg')

inRogueNotPG <-
  rog %>%
  anti_join(pg)

inPGNotRogue <-
  pg %>%
  anti_join(rog)

library(glue)

query <- glue_sql(
  "SELECT *
  from rogue.signups s
  where s.id IN ({signups*})",
  signups = inPGNotRogue$signup_id,
  .con = pg
)

qres <- runQuery(query,'pg')
