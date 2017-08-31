source('config/init.R')
source('config/mySQLConfig.R')

gotv <- read_csv('Data/RTV_DoSomethingOrg_APPENDED_20170724.csv')

q <- "SELECT DISTINCT u.northstar_id, u.mobile FROM quasar.users u WHERE u.mobile IS NOT NULL"

norths <- runQuery(q)