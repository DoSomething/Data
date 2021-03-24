# https://trello.com/c/NF4CG9zy/1440-data-request-consolidate-vroom-momt-data
source('config/init.R')
library(openxlsx)

dlist <- 
  seq.Date(as.Date('2018-01-01'),as.Date('2019-01-16'),by = 'day') %>% 
  gsub('-','',.)

dlist <- 
  dlist[!dlist %in% c('20180601','20180602')]

outSet <- tibble()

for (i in 1:length(dlist)) {
  print(i)
  f <- 
    suppressMessages(suppressWarnings(
      read_delim(paste0('Data/MOMTFEED/MO_MT_BFF_',dlist[i],'.txt'),'|')
      )) %>% 
    as.tibble() %>% 
    mutate(
      ind = dlist[i],
      MESSAGE_TIME = as.character(MESSAGE_TIME),
      ISMT = as.integer(ISMT),
      ISSUCCESS = as.integer(ISSUCCESS),
      CAMPAIGN_NAME = as.character(CAMPAIGN_NAME),
      POLL_NAME = as.character(POLL_NAME)
    )
  
  outSet %<>%
    bind_rows(f)
  
}

outSet %<>%
  mutate(monthYear = as.factor(substr(MESSAGE_DATE,1,6)))

allSets <- split(outSet, outSet$monthYear)

wb <- createWorkbook()

for (i in 1:length(allSets)) {
  
  writeSet <- 
    allSets[[i]] %>% 
    select(-ind,-monthYear)
  
  addWorksheet(wb, names(allSets[i]))
  writeData(wb, names(allSets[i]), writeSet, rowNames = F)
}

saveWorkbook(
  wb,
  paste0('Data/MOMTFEED/allSets.xlsx'),
  overwrite = TRUE
)
