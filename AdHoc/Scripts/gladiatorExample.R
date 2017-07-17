library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)
source('Scripts/DSUtils.R')

comp <- tbl_dt(read.csv('Data/contest_28-competition_209-users.csv'))
qres <- tbl_dt(read.csv('Data/query_result (5).csv'))

combine <-
  qres %>%
  left_join(comp, by = 'Northstar.ID') %>%
  mutate(
    reportback_for_contest = ifelse(is.na(Number.of.Posts) | Number.of.Posts == 0, 0, Number.of.Posts),
    reportback = ifelse(is.na(Number.of.Posts) | Number.of.Posts == 0, 0, 1)
  ) %>%
  filter(
    !is.na(Email)
  ) %>%
  summarise(
    avgNumberPosts = mean(Number.of.Posts),
    reportbackRate = mean(reportback)
  )

write.csv(combine, 'Data/output.csv')
