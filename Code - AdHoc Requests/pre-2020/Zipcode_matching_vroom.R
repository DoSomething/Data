library(tidyverse)
library(lettercase)
library(scales)
library(openxlsx)
library(data.table)

#TMI data request https://trello.com/c/EVv4Jv7B/1305-matching-user-zip-codes-with-location-and-socioeconomic-status

#import Vroom zipcodes
vroom_zips <- read.csv('~/Documents/Data Requests/Zip code matching TMI/Vroom_June2018_users_analysis.csv')%>%
  mutate(zipcode=as.character(zipcode))

#import second Vroom zipcodes
vroom_zips_2 <- read.csv('~/Documents/Data Requests/Zip code matching TMI/User_Zipcodes_-_Vroom_matching_user_zipcodes.csv.csv')%>%
  mutate(zipcode=as.character(zipcode))

#Pull Sohaib's zipcode/socioeconomic status code
irs <-
  read_csv('~/Documents/Data Requests/Footlocker/IRS 2015 data.csv', col_types = list( "zipcode" = col_character())) %>%
  setNames(c('state','zipcode','income', 'returns','n_people_hh','n_people_soc_tier','garbage1','notes','garbage2')) %>%
  select(-garbage1, -garbage2, -notes) %>%
  mutate(
    zipcode = as.character(
      ifelse(nchar(zipcode)==4, paste0('0',zipcode),
             ifelse(nchar(zipcode)==3, paste0('00',zipcode), zipcode)
      )
    )
  )

weightedIncome <-
  irs %>%
  group_by(zipcode) %>%
  summarise(
    weightedAverageIncome = round(weighted.mean(income, w=returns)),
    state = max(state)
  ) %>%
  mutate(
    socioeconomic_status =
      case_when(weightedAverageIncome == 1 ~ 'Poor',
                weightedAverageIncome == 2 ~ 'Low Income',
                weightedAverageIncome == 3 ~ 'Middle Income',
                weightedAverageIncome == 4 ~ 'Middle High',
                weightedAverageIncome == 5 ~ 'High Income',
                weightedAverageIncome == 6 ~ 'Super Income')
  )

zip<-weightedIncome%>%
  dplyr::select(zipcode, state, socioeconomic_status)

#Merge zipcodes and socio-economic categories with footlocker csv
vroom_zips_joined <-
  vroom_zips_2 %>%
  left_join(zip, by = 'zipcode')

write.csv(vroom_zips_joined, file = 'Vroom matching user zipcodes updated.csv')

write.csv(zip, file = 'Zipcodes with state and socio-economic status.csv')