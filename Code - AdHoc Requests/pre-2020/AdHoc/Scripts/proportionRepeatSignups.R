source('config/init.R')

signs <- 
  read_csv('Data/signsup_all_time.csv') %>% 
  setNames(c('northstar_id','ts')) %>% 
  group_by(northstar_id) %>% 
  mutate(
    firstSignup = if_else(ts == min(ts), 1, 0)
  ) %>% 
  filter(ts >= '2017-01-01')

table(signs$firstSignup)/nrow(signs)


# breakdown <- 
#   signs %>% 
#   group_by(firstSignup) %>% 
#   summarise(
#     count = n()
#   ) %>%
#   mutate(
#     proportion = count / sum(count)
#   )
