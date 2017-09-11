# https://trello.com/c/TIAiPTbB/1130-data-request-of-members-18-in-nov-2018
source('config/init.R')
source('config/DSUtils.R')
source('config/mySQLConfig.R')

q <- "SELECT u.northstar_id, u.birthdate FROM quasar.users u"

dat <- runQuery(q)

dat %>%
  mutate(
    birthdate = as.Date(birthdate),
    daysOld = Sys.Date() - birthdate,
    daysOldInNovember2018 = daysOld + (as.Date('2018-09-30') - Sys.Date()),
    ageNov2018 = as.numeric(floor(daysOldInNovember2018 / 365))
  ) %>% 
  filter(
    ageNov2018 > 18 & birthdate >= '1930-01-01'
  ) %>% 
  summarise(Count = n()) %>% 
  as.numeric() -> count