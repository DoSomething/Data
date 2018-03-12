source('config/init.R')
source('config/mySQLConfig.R')
library(zipcode)
library(ggmap)

flsa <- 
  read_csv('Data/3-2-18-FL-external-all-apps.csv') %>% 
  mutate(
    zip=substr(zip,1,5)
  ) %>% 
  left_join(zipcode, by='zip')

usa <- get_map(location = 'USA', zoom=4)

ggmap(usa, extent = "device") + 
  geom_density2d(data = flsa, 
                 aes(x = longitude, y = latitude), size = 0.3) + 
  stat_density2d(data = flsa, 
                 aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 1), guide = FALSE) +
  theme(legend.position="none")
