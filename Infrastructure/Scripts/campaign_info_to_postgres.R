source('config/init.R')

q <- "select * from quasar.campaign_info"

inf <- runQuery(q, 'mysql')

channel <- pgConnect()

if(dbExistsTable(channel,c("public", "campaign_info_postgres"))) {
  
  dbRemoveTable(channel,c("public", "campaign_info_postgres"))
  
}

dbWriteTable(channel,c("public", "campaign_info_postgres"), inf, row.names=F)

grant <- "grant select on campaign_info_postgres to public;"
dbGetQuery(channel, grant)

drop <- "drop table if exists public.campaign_info"
dbGetQuery(channel, drop)
