#https://trello.com/c/XuwtYkBb/1142-data-request-social-reach-member-count-in-brazil-and-the-uk

q <- "
SELECT 
  u.northstar_id,
  u.mobile,
  u.country
FROM quasar.users u
"

dat <- runQuery(q)

mzips <- "
SELECT 
  m.phone_number as mobile,
  m.country
FROM users_and_activities.mobile_users m
"

mzips <- runQuery(mzips)

comb <- 
  dat %>% 
  left_join(
    mzips %>% filter(!duplicated(mobile)), by = 'mobile'
  ) %>%
  mutate(
    country = ifelse(is.na(country.x) | country.x == '', country.y,
                     ifelse(is.na(country.y) | country.y == '', country.x, 
                            ifelse(!is.na(country.x) & country.x != '',country.x, NA)))
  ) %>% 
  group_by(
    country
  ) %>% 
  summarise(
    Count = n()
  )
