source('config/init.R')
source('config/mySQLConfig.R')
source('config/pgConnect.R')

q <- "select * from quasar.monitoring"

con <- quasarConnect()
inf <- runQuery(q, 'mysql')

inf <-
  inf %>%
  mutate(
    table = gsub('quasar', 'public', table),
    query = paste0('derived_', query)
  )

channel <- pgConnect()

if(dbExistsTable(channel, c("etl_monitoring","monitoring"))) {

  dbRemoveTable(channel, c("etl_monitoring","monitoring"))

}

dbWriteTable(channel,c("etl_monitoring","monitoring"), inf, row.names=F)

grant <- "grant select on etl_monitoring.monitoring to public;"
dbGetQuery(channel, grant)


