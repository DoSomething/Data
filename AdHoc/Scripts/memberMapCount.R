# https://trello.com/c/5VZGgRmF/1245-data-request-heatmap-of-ds-users
# https://trello.com/c/nrivFpBZ/1247-data-request-member-data-in-markets
source('config/init.R')
source('config/mySQLConfig.R')
library(zipcode)
library(ggmap)
library(eeptools)

data("zipcode")

q <- "
SELECT
  u.northstar_id,
  u.birthdate,
  u.addr_zip as zipcode
FROM quasar.users u
WHERE u.customer_io_subscription_status = 'subscribed' 
      OR u.sms_status = 'active'
"

qres <- runQuery(q, 'mysql')
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
us <- 
  qres %>% 
  mutate(
    zip = substr(zipcode, 1, 5),
    birthdate = as.Date(substr(birthdate, 1, 10)),
    age = age(birthdate),
    age18.25 = ifelse(age >= 18 & age <= 25, T, F)
  ) %>% 
  inner_join(zipcode)

pctMissing.dob <- sum(is.na(us$age)) / nrow(us)
pctMissing.loc <- 1 - nrow(us)/nrow(qres)

cityList <- c('Boston', 'New York', 'Baltimore', 'Orlando', 'Detroit',
              'Saint Louis', 'Springfield', 'New Orleans', 'Dallas', 'Honolulu',
              'Des Moines', 'Denver', 'Phoenix', 'Los Angeles', 'Portland')
top20 <- 
  us %>% 
  group_by(city) %>% 
  summarise(
    verfiedMembers = n(),
    expectMembers = applyPctChange(n(), pctMissing),
    verfied18.25 = sum(age18.25, na.rm=T),
    expect18.25 = applyPctChange(sum(age18.25, na.rm=T), pctMissing.dob)
  ) %>% 
  arrange(-verfiedMembers) %>% 
  filter(city %in% cityList)


usa <- get_map(location = 'USA', zoom=4)

ggmap(usa, extent = "device") + 
  geom_density2d(data = us, 
                 aes(x = longitude, y = latitude), size = 0.6) + 
  stat_density2d(data = us, 
                 aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.03, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 1), guide = FALSE) +
  theme(legend.position="none")