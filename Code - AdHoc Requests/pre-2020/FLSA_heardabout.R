library(tidyverse)
library(plyr)
library(dplyr)

flsa<-read.csv('~/Documents/Data Requests/Footlocker/FLSA data visualization/3-2-18-FL-external-all-apps (1).csv')

prop_heard<-flsa %>% group_by(hear_about) %>%
  ::dplyr::summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )
