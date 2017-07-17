library(tidyverse)
library(data.table)
library(dtplyr)

churn <- read_csv('Data/do_something_master_campaign_opt_ins_opt_outs_cumlative_20170627_152239.csv')

churn <- 
  churn %>%
  mutate(
    Date = as.Date(Date, "%m/%d/%y")
  ) %>%
  mutate(
    year = year(Date),
    month = month(Date),
    quarter = quarter(Date),
    active = opt_in - opt_out,
    total_opt_out = opt_out,
    total_opt_in = opt_in,
    opt_in = total_opt_in - lag(total_opt_in, n = 1),
    opt_out = total_opt_out - lag(total_opt_out, n = 1)
  ) 

quart <- 
  churn %>%
  group_by(year, quarter) %>%
  summarise(
    sum_out = sum(opt_out),
    sum_in = sum(opt_in),
    delta = sum(opt_in) - sum(opt_out)
  ) %>%
  filter(year >= 2014)

yr <- 
  churn %>%
  group_by(year) %>%
  summarise(
    sum_out = sum(opt_out),
    sum_in = sum(opt_in),
    delta = sum(opt_in) - sum(opt_out)
  ) %>%
  filter(year >= 2014)
