library(tidyverse)
library(stringr)

file = '~/Desktop/gambitShit.csv'

dat <-
  read.csv(file) %>%
  setNames('garbage') %>%
  filter(grepl('text', garbage)) %>%
  mutate(
    email = str_trim(gsub('"', '', gsub('"text" : "', '', garbage)))
  ) %>%
  select(email)

write_csv(dat, '~/Desktop/summitEmails.csv')

