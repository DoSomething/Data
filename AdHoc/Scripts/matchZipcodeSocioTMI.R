# https://trello.com/c/u0XUrONv/1187-matching-user-zip-code-with-location-and-socioeconomic-status
source('config/init.R')
source('config/mySQLConfig.R')
library(scales)
library(openxlsx)

irs <- 
  read_csv('Data/IRS-2015-data.csv', col_types = list( "zipcode" = col_character())) %>% 
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

vroom <- 
  openxlsx::read.xlsx('Data/Vroom_Nov_users_analysis.xlsx') %>% 
  setNames(c('phone_number','zipcode','state','socioeconomic_status')) %>% 
  select(-state, -socioeconomic_status) %>% 
  mutate(
    zipcode = as.character(ifelse(nchar(zipcode)==4, paste0('0',zipcode), zipcode))
  )

output <- 
  vroom %>% 
  left_join(weightedIncome)

saveCSV(output, desktop=T)
