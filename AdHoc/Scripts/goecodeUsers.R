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

q <- "
SELECT 
  u.northstar_id,
  u.addr_zip
FROM quasar.users u
WHERE u.addr_zip IS NOT NULL
AND u.addr_zip <> ''
AND u.country = 'US'
"

data <- runQuery(q)

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
# county_latlong <-
#   map_data('county') %>%
#   mutate(
#     county_name = toupper(subregion)
#   ) %>%
#   select(long, lat, county_name, group) %>%
#   tbl_dt()
# 
# data(counties)
# county_fips <-
#   counties %>%
#   select(county_name, county_fips) %>%
#   mutate(
#     county_name = toupper(gsub(' County', '', county_name))
#   ) %>%
#   tbl_dt() %>%
#   filter(!duplicated(county_name))

data(zip.regions)
pop <- 
  read_csv('Data/2010+Census+Population+By+Zipcode+(ZCTA).csv') %>% 
  setNames(c('region', 'population')) %>% tbl_dt() %>% 
  group_by(region) %>% 
  summarise(population = max(population))

zips <-
  data %>%
  mutate(
    zip = clean.zipcodes(addr_zip)
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
    estMembers = Count*15,
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
