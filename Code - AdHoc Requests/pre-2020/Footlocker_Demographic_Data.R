library(tidyverse)
library(lettercase)
library(scales)
library(openxlsx)
library(data.table)

footlocker <- read.csv('~/Downloads/edited_full_app_data (2).csv')

footlocker <- footlocker %>% rename(zipcode = zip)

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
  dplyr::select(zipcode, socioeconomic_status)

# #rename zipcode 'zip' to join with footlocker
# names(zip)[names(zip) == 'zipcode']<- 'zip'
#Merge zipcodes and socio-economic categories with footlocker csv
footlocker_zip <-
  footlocker %>%
  left_join(zip, by = 'zipcode')

write.csv(footlocker_zip, file = 'Footlocker Demographic Data.csv')