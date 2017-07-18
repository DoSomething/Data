library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

# df <- tbl_dt(read_csv('Data/the_all_inclusive member_event_log 2017-07-07T1316.csv'))
# 
# names(df) <- c('Uid','event_date','event_id','action_type','action_id','event_ts')

df <- tbl_dt(read_csv('~/Downloads/the_all_inclusive member_event_log 2017-07-17T1748.csv'))
names(df) <- c('Uid','event_date','event_id','action_type','action_id','event_ts')

df <-
  df %>%
  mutate(
    event_month = month(event_date),
    userMonth = paste0(Uid, '-', month(event_date))
  )

Users <- 
  df[
  ,
  .(
    activity = 1
  )
  ,
  by = .(Uid, event_month)
]

potential_activity <- tbl_dt(expand.grid(Uid = unique(Users$Uid), event_month = unique(Users$event_month)))

potential_activity <- 
  potential_activity %>%
  left_join(Users) %>%
  mutate(
    activity = ifelse(is.na(activity), 0, activity)
  ) 

potential_activity <- 
  potential_activity[
  ,
  previous_month_activity := lag(activity)
  ,
  by = Uid
]

potential_activity <- 
  potential_activity[,
    active_MoverM := ifelse(activity == 1 & previous_month_activity == 1, 1, 0)
  ]

rates <- potential_activity[, sum(active_MoverM) / sum(previous_month_activity) , by = event_month]

