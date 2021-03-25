source('config/init.R')

q <- getQuery('Scripts/MAMDive.sql')
q <- gsub('TODAY_DATE', Sys.Date(),q)

qres <- runQuery('Scripts/MAMDive.sql', 'mysql')


