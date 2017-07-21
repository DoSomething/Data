chimp <- tbl_dt(read_csv('~/Downloads/subscribed_members_export_33d3bb2d7a.csv'))
unsub <- tbl_dt(read_csv('~/Downloads/unsubscribed_members_export_33d3bb2d7a.csv'))

unsub.sub <- unsub %>% select(`Email Address`, `CONFIRM_TIME`, `UNSUB_TIME`,`Country Code`) %>% setNames(c('email','confirm_time_unsub','unsub_time', 'country'))
chimpSub <- chimp %>% select(`Email Address`, CONFIRM_TIME, LAST_CHANGED, `Country Code`) %>% setNames(c('email', 'confirm_time', 'last_changed', 'country'))

both <- 
  chimpSub %>%
  left_join(unsub.sub, by='email')

unsub.sub.unaltered <- unsub %>% select(`Email Address`, `CONFIRM_TIME`, `UNSUB_TIME`)
chimpSub.unaltered <- chimp %>% select(`Email Address`, CONFIRM_TIME, LAST_CHANGED)

unioned <- rbind(unsub.sub, chimpSub, fill=T)

unioned[,.N,by=country]

agg <- unioned[,.(count = .N), by=year(CONFIRM_TIME)][order(year)][,runningTotal := cumsum(count)]


# internation Omidyar Counts ----------------------------------------------
library(ISOcodes)
library(maps)
library(maptools)
library(raster)
library(rworldmap)
data("ISO_3166_1")
countryCodes <- ISO_3166_1 %>% tbl_dt() %>% select(Alpha_2, Name) %>% setNames(c('country', 'country_name'))

totalMembers = 5600000
interMembers = 200000

world <- map_data('world')

# intCount = read_csv('Data/internationalCounts.csv')
# intCount %<>%
#   mutate(
#     proportion = count / sum(count),
#     expectCount = proportion * interMembers
#   ) %>%
#   left_join(
#     countryCodes
#   )

read_csv('Data/chimpInterFullList.csv') %>%
  rbind(
    read_csv('Data/phoenixInternationEmails.csv'),
    read_csv('Data/cggInternationalUsers.csv')
  ) %>%
  group_by(email) %>%
  filter(row_number() == 1) %>%
  group_by(country) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    proportion = count / sum(count),
    expectCount = round(proportion * interMembers)
  ) %>%
  filter(!(country %in% c('US','USA'))) %>%
  left_join(
    countryCodes
  ) %>%
  filter(expectCount > 1) -> intCount

rworldOut <- joinCountryData2Map(intCount, nameJoinColumn = 'country_name', joinCode = "NAME")
# mapDevice('x11')
mapParams <- 
  mapCountryData(rworldOut, 
                 nameColumnToPlot='expectCount', 
                 catMethod='fixedWidth', 
                 numCats=50, 
                 mapTitle = 'International Member Heat Map')

do.call(addMapLegend, c(mapParams
                        ,legendLabels="all"
                        ,legendWidth=1.5)) 

rawCount <-
  intCount %>%
  filter(!is.na(country_name)) %>%
  select(country, country_name, expectCount) %>%
  setNames(c('Country','Country_Name', 'Count')) %>%
  arrange(-Count)

#####ggplot approach####
library(httr)
library(rgdal)
library(scales)
library(stringr)
library(stringi)

try(invisible(GET("http://www.pewglobal.org/wp-content/lib/js/world-geo.json",
                  write_disk("Data/world-geo.json"))), silent=TRUE)

world <- readOGR("Data/world-geo.json", "OGRGeoJSON")
world_wt <- spTransform(world, CRS("+proj=robin"))
world_map <- fortify(world_wt)

world_map %<>%
  left_join(data_frame(id=rownames(world@data), name=world@data$name)) %>%
  tbl_dt() %>%
  dplyr::rename(name = country_name)

read_csv("http://www.pewglobal.org/wp-content/themes/pew-global/interactive-global-class.csv") %>%
  mutate_each(funs(str_trim)) %>%
  filter(id != "None") %>%
  mutate_each(funs(as.numeric(.)/100), -name, -id) -> dat
dat %>%
  gather(share, value, starts_with("Share"), -name, -id) %>%
  select(-starts_with("Change")) %>%
  mutate(label=factor(stri_trans_totitle(str_match(share, "Share ([[:alpha:]- ]+),")[,2]),
                      c("Poor", "Low Income", "Middle Income", "Upper-Middle Income", "High Income"),
                      ordered=TRUE)) -> share_dat

poynter_scale_breaks <- c(0, 2.5, 5, 10, 25, 50, 75, 80, 100)

sprintf("%2.1f-%s", poynter_scale_breaks, percent(lead(poynter_scale_breaks/100))) %>%
  stri_replace_all_regex(c("^0.0", "-NA%"), c("0", "%"), vectorize_all=FALSE) %>%
  head(-1) -> breaks_labels

share_dat %>%
  mutate(`Share %`=cut(value,
                       c(0, 2.5, 5, 10, 25, 50, 75, 80, 100)/100,
                       breaks_labels))-> share_dat

share_pal <- c("#eaecd8", "#d6dab3", "#c2c98b", "#949D48", "#6e7537", "#494E24", "#BB792A", "#7C441C", "#ffffff")

raw <- ggplot() + geom_map(data=world_map, map=world_map,
                           aes(x=long, y=lat, group=group, map_id=id),
                           color="#7f7f7f", fill="white", size=0.15)

intSub <- intCount %>% filter(!is.na(country_name)) %>% mutate(expectCount1 = as.factor(expectCount))
filled <- raw + 
  geom_map(data=share_dat, map=world_map,
           aes(map_id = factor(name), fill=factor(`Share %`)),
           color="#7f7f7f", size=0.15)
