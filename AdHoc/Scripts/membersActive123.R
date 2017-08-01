library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

df <- tbl_dt(read_csv('Data/members_activated_may.csv')) %>%
  setNames(c('event_id','user_id','user_created_ts','action_id','action_type','event_ts', 'email', 'mobile')) %>%
  filter(!(is.na(email) & !is.na(mobile))) %>%
  mutate(
    event_month = month(as.Date(event_ts, format='%m/%d/%y'))
  ) %>%
  group_by(user_id) %>%
  mutate(
    activeMay = any(event_month==5),
    activeJune = any(event_month==6),
    activeJuly = any(event_month==7)
    ) %>%
  ungroup() %>%
  arrange(user_id, event_month) %>%
  filter(!duplicated(user_id)) %>%
  mutate(
    activeOnlyMay = ifelse(activeMay==T & activeJune==F & activeJuly==F, T, F),
    activeAllThree = ifelse(activeMay==T & activeJune==T & activeJuly==T, T, F),
    activeMayPlus2 = ifelse(activeMay==T & (activeJune==T | activeJuly==T) & activeAllThree==F, T, F)
  ) %>%
  select(user_id, user_created_ts, starts_with('active'))

saveCSV(df, desktop=T)
