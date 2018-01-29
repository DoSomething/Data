source('config/init.R')
library(zipcode)
library(ggmap)

map <- get_map(location = 'USA', zoom=4)

usa_center = as.numeric(geocode("United States"))

USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=3.92), extent="normal")

data('zipcode')

zipcode %<>%
  rename(zipcode = zip)

fl <- read_csv(file = '~/Downloads/Footlocker Final Total Scores - Footlocker Total Scores.csv')

fl %<>%
  select(Rank, first_name, last_name, zipcode) %>% 
  left_join(zipcode) %>% 
  filter(!state %in% c('AL','HI')) %>% 
  filter(!is.na(state)) %>% 
  filter(longitude > -140)



USAMap + 
  geom_point(aes(x=longitude, y=latitude), 
             data=fl, col="black", alpha=.7) 
