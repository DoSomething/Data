setwd('~/Data/Infrastructure/')
source('config/init.R')
source('config/mySQLConfig.R')
source('config/pgConnect.R')

q <- "select * from quasar.campaign_info"

con <- quasarConnect()
inf <- runQuery(q, 'mysql')

channel <- pgConnect()

if(dbExistsTable(channel,c("public", "campaign_info_postgres"))) {

  dbRemoveTable(channel,c("public", "campaign_info_postgres"))

}

dbWriteTable(channel,c("public", "campaign_info_postgres"), inf, row.names=F)

grant <- "grant select on campaign_info_postgres to public;"
dbGetQuery(channel, grant)
