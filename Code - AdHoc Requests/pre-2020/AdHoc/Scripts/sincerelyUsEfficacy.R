
all_campaigns <- 
  read_csv('Data/all_signups_after_6_06_2017.csv') %>%
  setNames(c('id', 'campaign_name','sign_up_run_nid','sign_up_sid','sign_up_ts')) %>%
  select(id, campaign_name, sign_up_ts) %>%
  filter(!is.na(id))
  
allCount <- 
  all_campaigns %>%
  group_by(id) %>%
  summarise(
    count = n()
  )

su <- 
  read_csv('Data/SincerelyUsMembers.csv') %>%
  select(id) %>%
  left_join(allCount) %>%
  mutate(
    count = ifelse(is.na(count), 1, count+1)
  )

aggregated <-
  su %>%
  mutate(
    moreThanOne = ifelse(count > 1, 1, 0),
    randomNumber = runif(nrow(su), 0, 1),
    peopleWeCareAbout = ifelse(randomNumber > .5, T, F)
  ) %>%
  summarise(
    ratio = mean(moreThanOne)
  )
  
su %<>%
  left_join(all_campaigns, by='id') %>%
  filter(!is.na(campaign_name)) %>%
  group_by(campaign_name) %>%
  summarise(
    count = n()
  ) %>%
  arrange(-count)
  
saveCSV(su, desktop=T)




