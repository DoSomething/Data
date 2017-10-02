source('config/init.R')
source('config/mySQLConfig.R')
library(zipcode)
library(noncensus)
data(zip_codes)
data(zipcode)

zipcode %<>% 
  tbl_df() %>% 
  mutate(zip = as.character(zip)) %>% 
  select(zip, city)

queens <- read_csv('Data/queens_neighborhoods.csv')

cityList <- c('Boston','Chicago','Philadelphia',
              'San Francisco','Los Angeles',
              'Denver','Washington','New York',
              'Queens','Bronx','Brooklyn','Raleigh',
              'Dallas','Atlanta','Miami')

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

dat <- runQuery(q)

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

zip <-
  dat %>% 
  mutate(
    addr_zip = as.character(clean.zipcodes(addr_zip)),
    mobile = cleanPhone(mobile)
  ) %>% tbl_df() %>% 
  left_join(cleanMobileZips, by = 'mobile') %>% 
  mutate(
    zip = ifelse(!is.na(addr_zip), addr_zip,
                 ifelse(!is.na(zip.m), zip.m, NA)),
    countryFinal = ifelse(!is.na(country) & !is.na(country.m), country,
                          ifelse(is.na(country), country.m,
                                 ifelse(is.na(country.m), country, NA)))
  ) %>% tbl_dt()

pctUS <- 1 - (length(which(!(zip$countryFinal %in% c('US','')))) / length(which(zip$countryFinal=='US')))

nrow(zip)*pctUS

zip %<>% 
  left_join(
    zipcode
  ) %>% 
  mutate(
    city = ifelse(city %in% queens$neighborhoods | 
                  city %in% c('New York','Queens','Bronx','Brooklyn'),
                  'New York', city)
  ) %>% 
  filter(!is.na(zip))

scale <- 1 / (nrow(zip) / nrow(dat))

zip %>% 
  filter(city %in% cityList) %>% 
  group_by(city) %>% 
  summarise(
    Count = n(),
    estCount = n()*as.numeric(scale)
  ) -> counts

