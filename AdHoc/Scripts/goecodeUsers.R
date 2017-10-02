#load up the ggmap library
source('config/init.R')
source('config/mySQLConfig.R')
library(ggmap)
library(ggthemes)
library(xlsx)
library(zipcode)
library(maps)
library(maptools)
library(noncensus)
library(choroplethr)
library(choroplethrZip)
library(choroplethrMaps)
library(scales)

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
                 ifelse(!is.na(zip.m), zip.m, NA)),
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

county <-
  read.xlsx('Data/DoSomething_CountyAgg_RAYSE.xlsx', sheetIndex = 1) %>% 
  tbl_dt() %>% 
  filter(!is.na(countyfp)) %>% 
  mutate(
    estMembers = Count*scale,
    estMembers = ifelse(is.na(estMembers), 0, estMembers),
    RAYSE_Scaled = (as.numeric(RAYSE_Rating)-1) / 10,
    proportion = estMembers / population,
    proportion = ifelse(proportion > 1, Count / population, proportion),
    proportion = ifelse(proportion == Inf, 0, proportion),
    proportion = ifelse(proportion > 1, 1 / population, proportion),
    proportion = ifelse(is.na(proportion), 0, round(proportion, 5)),
    proportionRank = rank(proportion)-9,
    scaledRank = scalerange(proportionRank),
    RAYSE_DS_Delta = scaledRank - RAYSE_Scaled
  )

ggplot(county, aes(x=scaledRank)) + geom_density() + geom_density(aes(x=RAYSE_Scaled))
ggplot(county, aes(x=proportion)) + geom_density()

Out <- 
  county %>% 
  select(countyfp, statefp, StateName, Countynamelsad, 
         estMembers, proportion, population, RAYSE_Rating, RAYSE_DS_Delta) %>% 
  mutate(
    Group = cut(RAYSE_DS_Delta, breaks = seq(-1,1,.25))
  ) 

Agged <-
  Out %>% 
  group_by(Group) %>% 
  summarise(
    County = n(),
    Population = sum(population),
    Member = sum(estMembers)
  ) %>% 
  mutate(
    County = County / sum(County),
    Population = Population / sum(Population),
    Member = Member / sum(Member),
    Group = ifelse(is.na(Group), 'Unknown', as.character(Group)),
    Group = factor(
      Group, 
      levels = c('Unknown','(-1,-0.75]', '(-0.75,-0.5]', '(-0.5,-0.25]', 
                 '(-0.25,0]','(0,0.25]','(0.25,0.5]','(0.5,0.75]','(0.75,1]')),
    Order = as.numeric(Group)
  ) %>% 
  arrange(Group, Order) %>%
  mutate(Order=NULL) %>% 
  gather(Group, 'Percentage') %>% 
  setNames(c('Bucket', 'Type', 'Percentage')) %>% 
  group_by(Type) %>% 
  mutate(
    Text = percent(Percentage),
    Position = cumsum(Percentage) - (0.5 * Percentage)
  )

ggplot(Agged, aes(x=Type, y=Percentage, fill=Bucket)) +
  geom_bar(stat='identity', position='stack', width=.66) +
  geom_text(aes(label=Text, y=1-Position), size=2.5) +
  labs(title = 'Population Allocation by RAYSE <-> Member Concentration Delta',
       x='', y='Proportion') + 
  scale_y_continuous(breaks=pretty_breaks(10)) + 
  theme(plot.title = element_text(hjust = 0.5))

saveCSV(Out, desktop=T)
saveCSV(Agged, desktop=T)

mapCounty <-
  county %>%
  mutate(
    region = as.numeric(paste0(statefp, countyfp)),
    value = RAYSE_DS_Delta
  ) %>% 
  filter(!is.na(value)) %>% 
  select(region, value)

county_choropleth(mapCounty)

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
# 
# data(df_pop_county) 
# county_choropleth(df_pop_county)

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
