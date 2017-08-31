source('config/init.R')
source('config/mySQLConfig.R')

q <- "SELECT * FROM quasar.campaign_activity LIMIT 10"

t <- runQuery.s(q)