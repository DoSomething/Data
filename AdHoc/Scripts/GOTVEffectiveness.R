source('config/init.R')
source('config/mySQLConfig.R')

rtv <- read_csv('Data/RTV_DoSomethingOrg_APPENDED_20170724.csv')

uid <- prepQueryObjects(rtv$PHONE)

q <- paste0(
  "SELECT 
    c.*
  FROM quasar.users u 
  LEFT JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
  WHERE u.mobile IN ", uid)

cam <- runQuery(q)
