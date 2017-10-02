source('config/init.R')
source('config/mySQLConfig.R')
library(zipcode)

q <- "
SELECT 
u.northstar_id,
u.mobile,
u.addr_zip,
u.country
FROM quasar.users u
WHERE (u.customer_io_subscription_status = 'subscribed' 
OR u.moco_current_status = 'active')
"

qres <- runQuery(q)

mzips <- "
SELECT 
m.phone_number as mobile,
m.zip,
m.country
FROM users_and_activities.mobile_users m
WHERE m.zip IS NOT NULL
AND m.status = 'Active Subscriber'"

mzips <- runQuery(mzips)

cleanMobileZips <- 
  mzips %>% 
  mutate(
    mobile = cleanPhone(mobile),
    zip.m = as.character(clean.zipcodes(zip)),
    country.m = country
  ) %>% 
  select(mobile, zip.m, country.m)

dat <- 
  qres %>% 
  mutate(
    addr_zip = as.character(clean.zipcodes(addr_zip)),
    mobile = cleanPhone(mobile)
  ) %>% tbl_df() %>% 
  left_join(cleanMobileZips, by = 'mobile') %>% 
  mutate(
    zip = ifelse(!is.na(addr_zip), addr_zip,
                 ifelse(!is.na(zip.m), zip.m, NA))
  ) %>% 
  select(northstar_id, zip) 

scale <- 1 / (sum(!is.na(dat$zip))/nrow(dat))

data(zipcode)

stateList <- c('AZ','CO','KY','NV','PA','VA','WI','WV')

stateDens <- 
  zipcode %>% 
  tbl_df() %>% 
  select(zip, state, city) %>% 
  filter(state %in% stateList) %>% 
  inner_join(dat, by='zip') %>% 
  group_by(state, zip) %>% 
  summarise(zipCount = n()) %>% 
  group_by(state) %>% 
  filter(zipCount == max(zipCount)) %>% 
  mutate(zipCount = round(zipCount * scale)) %>% 
  left_join(
    zipcode %>% select(zip, city)
  )
