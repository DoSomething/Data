source('config/init.R')
source('config/mySQLConfig.R')

q <- "
  SELECT 
    c.northstar_id,
    u.mobile as phone_number,
    max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) as reportedback
  FROM quasar.campaign_activity c 
  LEFT JOIN quasar.users u ON u.northstar_id = c.northstar_id
  WHERE c.campaign_run_id=7931
  GROUP BY c.northstar_id"

rb <- 
  runQuery(q) %>% 
  filter(reportedback == 1) %>% 
  mutate(phone_number = cleanPhone(phone_number))

textin <- 
  read_csv('Data/rideandseek_posters_group_20171026_143136.csv') %>% 
  select(phone_number) %>% 
  mutate(phone_number = cleanPhone(phone_number))

length(which(textin$phone_number %in% rb$phone_number))

# patterns <- list(
#   x / 5 == 1 ~ "fizz",
#   x /  7 == 1 ~ "buzz",
#   x / 35 == 1 ~ "fizz buzz",
#   TRUE ~ as.character(x)
# )
# print(patterns)
# x <- 1:50
# print(x)
# 
# case_when(!!! patterns)
