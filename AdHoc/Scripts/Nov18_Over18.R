# https://trello.com/c/TIAiPTbB/1130-data-request-of-members-18-in-nov-2018
source('config/init.R')
source('config/DSUtils.R')
source('config/mySQLConfig.R')

q <- "SELECT u.northstar_id, u.birthdate FROM quasar.users u"

dat <- runQuery(q)

# test <-
  dat %>%
  mutate(
    birthdate = as.Date(birthdate),
    daysOld = Sys.Date() - birthdate,
    daysOldInNovember2018 = daysOld + (as.Date('2018-11-07') - Sys.Date()),
    ageNov2018 = as.numeric(floor(daysOldInNovember2018 / 365.25)),
    ageToday = as.numeric(floor(daysOld / 365.25))
  ) %>%
  filter(
    ageNov2018 >= 18 & ageToday < 18
  ) %>%
  summarise(Count = n()) %>%
  as.numeric() -> count