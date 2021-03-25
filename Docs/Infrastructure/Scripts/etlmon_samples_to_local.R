source('config/pgConnect.R')
source('config/init.R')
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
localchannel <-
  dbConnect(
    drv,
    user = 'root',
    password='password',
    dbname = 'dbname=postgres',
    host = "localhost",
    port = 5566
  )

channel <- pgConnect()

q <- "select * from northstar.users limit 100"
nu <- runQuery(q,'pg')
dbWriteTable(localchannel,c("northstar","users"), nu, row.names=F)

q <- "select * from rogue.signups limit 100"
rs <- runQuery(q,'pg')
dbWriteTable(localchannel,c("rogue","signups"), rs, row.names=F)

q <- "select * from rogue.posts limit 100"
rp <- runQuery(q,'pg')
dbWriteTable(localchannel,c("rogue","posts"), rp, row.names=F)

q <- "select * from puck.events limit 100"
pe <- runQuery(q,'pg')
dbWriteTable(localchannel,c("puck","events"), pe, row.names=F)

q <- "select * from cio.email_event limit 100"
ce <- runQuery(q,'pg')
dbWriteTable(localchannel,c("cio","email_event"), ce, row.names=F)

q <- "select * from cio.customer_event limit 100"
cc <- runQuery(q,'pg')
dbWriteTable(localchannel,c("cio","customer_event"), cc, row.names=F)

q <- "select * from public.users limit 100"
du <- runQuery(q,'pg')
dbWriteTable(localchannel,c("public","users"), du, row.names=F)

q <- "select * from public.campaign_activity limit 100"
dca <- runQuery(q,'pg')
dbWriteTable(localchannel,c("public","campaign_activity"), dca, row.names=F)

q <- "select * from public.phoenix_sessions limit 100"
dps <- runQuery(q,'pg')
dbWriteTable(localchannel,c("public","phoenix_sessions"), dps, row.names=F)

q <- "select * from public.phoenix_events limit 100"
dpe <- runQuery(q,'pg')
dbWriteTable(localchannel,c("public","phoenix_events"), dpe, row.names=F)

q <- "select * from etl_monitoring.monitoring limit 100"
mon <- runQuery(q,'pg')
dbWriteTable(localchannel,c("etl_monitoring","monitoring"), mon, row.names=F)

