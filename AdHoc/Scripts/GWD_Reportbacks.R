source('config/init.R')
source('config/DSUtils.R')
source('config/mySQLConfig.R')
#https://trello.com/c/VxC2bLKq/1129-data-request-list-of-phone-numbers-from-northstar-ids

dat <- 
  read_csv('Data/2016_GWD_Reportbacks.csv') 

val <- colnames(dat) %>% tbl_dt() %>% setnames(colnames(dat))

dat %<>%
  bind_rows(val) %>% 
  setnames('northstar_id')

ns <- prepQueryObjects(dat$northstar_id)

q <- paste0(
  'SELECT u.northstar_id, u.mobile FROM quasar.users u WHERE u.northstar_id IN', ns
)

out <- runQuery(q)

saveCSV(out, desktop=T)
