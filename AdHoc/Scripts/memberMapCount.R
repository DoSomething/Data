# https://trello.com/c/5VZGgRmF/1245-data-request-heatmap-of-ds-users
# https://trello.com/c/nrivFpBZ/1247-data-request-member-data-in-markets
source('config/init.R')
source('config/mySQLConfig.R')
library(zipcode)
library(ggmap)

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

us <- 
  qres %>% 
  mutate(
    zip = substr(zipcode, 1, 5)
  ) %>% 
  inner_join(zipcode)

top20 <- 
  us %>% 
  group_by(city) %>% 
  summarise(
    Count = n()
  ) %>% 
  arrange(-Count) 

samp <- 
  us %>% 
  sample_frac(.2)

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