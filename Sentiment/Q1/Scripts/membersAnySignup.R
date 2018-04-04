source('config/init.R')
source('config/customFunctions.R')
source('config/mySQLConfig.R')

qres <- runQuery('Q1/Scripts/membersAnySignup.sql', 'mysql')

su <-
  qres %>%
  mutate(
    group =
      case_when(
        niche==1 ~ 'niche',
        sms_only==1 ~ 'sms_only',
        TRUE ~ 'all_others'
      )
  ) %>%
  group_by(group, has_signup) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    p = n / sum(n)
  )

