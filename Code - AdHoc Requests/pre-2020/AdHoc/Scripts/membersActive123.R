library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

df <- 
  read_csv('Data/members_activated_june.csv') %>%
  tbl_dt() %>% 
  setNames(c('row_num','event_id','user_id','user_created_ts','action_id','action_type','event_ts', 'email', 'mobile')) %>%
  filter(!(is.na(email) & !is.na(mobile))) %>%
  mutate(
    event_month = month(as.Date(event_ts, format='%m/%d/%y'))
  ) %>%
  group_by(user_id) %>%
  mutate(
    activeJune = any(event_month==6),
    activeJuly = any(event_month==7),
    activeAugust = any(event_month==8)
    ) %>%
  ungroup() %>%
  arrange(user_id, event_month) %>%
  filter(!duplicated(user_id)) %>%
  mutate(
    activeOnlyJune = ifelse(activeJune==T & activeJuly==F & activeAugust==F, T, F),
    activeAllThree = ifelse(activeJune==T & activeJuly==T & activeAugust==T, T, F),
    activeJunePlus2 = ifelse(activeJune==T & (activeJuly==T | activeAugust==T) & activeAllThree==F, T, F)
  ) %>%
  select(user_id, user_created_ts, starts_with('active'))

saveCSV(df, desktop=T)
