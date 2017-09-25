#load up the ggmap library
source('config/init.R')
source('config/mySQLConfig.R')
library(ggmap)
library(ggthemes)
library(zipcode)
library(maps)
library(maptools)
library(noncensus)
library(choroplethr)
library(choroplethrZip)
library(choroplethrMaps)

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

dat %<>% 
  mutate(
    addr_zip = as.character(clean.zipcodes(addr_zip)),
    mobile = cleanPhone(mobile)
  ) %>% tbl_df() %>% 
  left_join(cleanMobileZips, by = 'mobile') %>% 
  mutate(
    zip = ifelse(is.na(addr_zip), zip.m,
                 ifelse(is.na(zip.m), addr_zip, NA)),
    countryFinal = ifelse(!is.na(country) & !is.na(country.m), country,
                          ifelse(is.na(country), country.m,
                                 ifelse(is.na(country.m), country, NA)))
  ) %>% 
  select(northstar_id, mobile, zip, countryFinal) 

scale <- 1 / (sum(!is.na(dat$zip))/nrow(dat))

data(zip.regions)
pop <- 
  read_csv('Data/2010+Census+Population+By+Zipcode+(ZCTA).csv') %>% 
  setNames(c('region', 'population')) %>% tbl_dt() %>% 
  group_by(region) %>% 
  summarise(population = max(population))

zips <-
  dat %>%
  mutate(
    zip = clean.zipcodes(zip)
  ) %>% 
  group_by(zip) %>% 
  summarise(
    Count = n()
  ) %>% 
  transmute(
    region = zip,
    Count = Count
  )

mapthis <- 
  zip.regions %>% 
  filter(!duplicated(region)) %>% 
  select(region) %>% 
  left_join(zips) %>% 
  inner_join(pop) %>% 
  mutate(
    estMembers = Count*scale,
    estMembers = ifelse(is.na(estMembers), 0, estMembers),
    value = estMembers / population,
    value = ifelse(value == Inf, 0, value),
    value = ifelse(value > 1, Count / population, value),
    value = ifelse(value > 1, 1 / population, value),
    value = ifelse(is.na(value), 0, round(value, 5))
  ) %>% tbl_dt()

zip_choropleth(
  mapthis, 
  title = 'Proportion of Do Something Members per Zip Code', 
  legend = 'Proportion')

# zip_county <- read_csv('Data/zip_county.csv')
# 
# countyCounts <-
#   zips %>% 
#   left_join(zip_county, by = c('region' = 'ZIP'))

# data(zipcode)
# zipcode %<>% tbl_dt()
# map<-get_map(location='united states', zoom=4, maptype = 'roadmap',
#              source='google',color='color')
# ggmap(map)
# 
# zips <-
#   data %>%
#   mutate(
#     zip = clean.zipcodes(addr_zip)
#   ) %>%
#   inner_join(zipcode) %>%
#   group_by(latitude, longitude) %>%
#   summarise(
#     Count = n()
#   )
# 
# ggmap(map) + 
#   stat_density2d(data=zips, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..), bins=32, geom='polygon') +
#   scale_fill_gradient(low='green', high='red', guide=FALSE) + 
#   scale_alpha(range = c(0,1), guide=FALSE) + 
#   labs(x='Longitude', y='Latitude', title='Heatmap of DoSomething.org Members')
# 
# ggplot() + 
#   geom_polygon(data=zips, aes(x=longitude, y=latitude, fill=Count)) + 
#   coord_map() + 
#   scale_fill_distiller(name="Percent", palette = "YlGn", breaks = pretty_breaks(n = 5))+
#   theme_nothing(legend = TRUE)
# zip_fips <-
#   read_csv('../../Downloads/zcta_county_rel_10.csv') %>%
#   select(ZCTA5, COUNTY) %>%
#   transmute(
#     zip = ZCTA5,
#     county_fips = COUNTY
#   ) %>%
#   filter(!duplicated(zip))
# 
county_latlong <-
  map_data('county') %>%
  mutate(
    county_name = toupper(subregion)
  ) %>%
  select(long, lat, county_name, group) %>%
  tbl_dt()

data(counties)
county_fips <-
  counties %>%
  select(county_name, county_fips) %>%
  mutate(
    county_name = toupper(gsub(' County', '', county_name))
  ) %>%
  tbl_dt() %>%
  filter(!duplicated(county_name))

data(df_pop_county) 
county_choropleth(df_pop_county)

# ready <-
#   zips %>% 
#   inner_join(zip_fips, copy=T) %>% 
#   inner_join(county_fips) %>% 
#   tbl_dt() %>% 
#   select(county_name, Count) %>% 
#   transmute(
#     region = county_fips,
#     value = Count
#   )
# 
# ggplot(map.county, aes(x=long, y=lat, group=group)) + geom_polygon() + coord_map()
