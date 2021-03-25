library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

df <- tbl_dt(read_csv('Data/freddie_monthlyRetentionLook 2017-09-21.csv'))
names(df) <- c('Uid','event_date','event_id','action_type','action_id')

df <-
  df %>%
  mutate(
    event_month = month(event_date),
    event_time_window = ifelse( inInterval(Sys.Date() - event_date, c(0,30)), '0-30',
                              ifelse(inInterval(Sys.Date() - event_date, c(31,60)), '31-60', '61-90')),
    userMonth = paste0(Uid, '-', month(event_date))
  )

getRates <- function(df, pivot) {
  
  Users <- 
    df[
      ,
      .(
        activity = 1
      )
      ,
      by = c('Uid', pivot)
      ]
  
  potential_activity <- tbl_dt(
    expand.grid(
      Uid = unique(Users$Uid), 
      pivot = pull(unique(Users[,.(get(pivot))]))
      )
    ) 
  
  setnames(potential_activity, 'pivot', paste(pivot))
  
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
    potential_activity[
      ,
      activePoverP := ifelse(activity == 1 & previous_month_activity == 1, 1, 0)
      ]
  
  rates <- 
    potential_activity[
      , 
      .(
        activePoverP = sum(activePoverP) / sum(previous_month_activity)), 
      by = c(pivot)
      ]
  
  return(rates)
}

getRates(df, 'event_time_window')
getRates(df, 'event_month')
