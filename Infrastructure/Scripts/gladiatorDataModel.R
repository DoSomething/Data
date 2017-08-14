library(tidyverse)
library(data.table)
library(dtplyr)
library(jsonlite)
library(xlsx)

glad <- fromJSON('Data/sample_response_from_gladiator-thor.json')

waitingRoom <-
  glad$data$waitingRoom$data %>%
  tbl_dt() %>%
  select(-users) %>%
  mutate(
    contest_id = as.numeric(glad$data$id),
    signup_date.start = glad$data$waitingRoom$data$signup_dates$start,
    signup_date.end = glad$data$waitingRoom$data$signup_dates$end
  ) %>%
  select(-signup_dates)

messages <-
  glad$data$messages$data[[1]] %>%
  tbl_dt() %>%
  select(-starts_with('data'), -starts_with('featured'), -starts_with('leaderboard')) %>%
  mutate(
    type.name = glad$data$messages$data[[1]]$type$name,
    type.key = glad$data$messages$data[[1]]$type$key
  ) %>%
  select(-type)

competitions <-
  glad$data$competitions$data %>%
  tbl_dt() %>%
  mutate(
    competition_dates.start = glad$data$competitions$data[[1]]$competition_dates$start_date,
    competition_dates.end = glad$data$competitions$data[[1]]$competition_dates$end_date,
    contest_id = glad$data$id
  ) %>%
  select(-competition_dates, -users, -subscribed_users, -unsubscribed_users)

contest <-
  glad$data %>%
  tbl_dt() %>%
  mutate(
    sender.name = glad$data$sender$name,
    sender.email = glad$data$sender$email
  ) %>%
  select(id, campaign_id, campaign_run_id, created_at, updated_at, sender.name, sender.email)

users <-
  data.table(user_id = glad$data$competitions$data[[1]]$users[[4]]) %>%
  tbl_dt() %>%
  mutate(
    contest_id = glad$data$id,
    competition_id =  glad$data$competitions$data[[1]]$id[1:3],
    waitingRoom_id = glad$data$waitingRoom$data$id,
    subscribed = c(T,F,T),
    unsubscribed = c(F,T,F)
  ) %>%
  mutate(
    testField = c(T, T, F)
  )

write.xlsx(contest, file = 'Data/gladiatorModel.xlsx', sheetName = 'contest', row.names = F)
write.xlsx(competitions, file = 'Data/gladiatorModel.xlsx', sheetName = 'competitions', row.names = F, append = T)
write.xlsx(waitingRoom, file = 'Data/gladiatorModel.xlsx', sheetName = 'waitingRoom', row.names = F, append = T)
write.xlsx(messages, file = 'Data/gladiatorModel.xlsx', sheetName = 'messages', row.names = F, append = T)
write.xlsx(users, file = 'Data/gladiatorModel.xlsx', sheetName = 'users', row.names = F, append = T)
