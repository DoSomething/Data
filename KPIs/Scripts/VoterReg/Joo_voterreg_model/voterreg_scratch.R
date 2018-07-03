############ Preparing mel data #################

mel_samp <-
  mel_cleaned %>%
  sample_frac(.1, replace = FALSE) %>%
  mutate(action_time = hour(timestamp)) %>%
  group_by(northstar_id) %>% 
  summarise_all(tally)

melsamp <- 
  melcleaned %>% 
  sample_frac(.1, replace = FALSE) %>%
  select(-c("timestamp", "source")) %>% 
  group_by(northstar_id, action_type) %>%
  tally %>% 
  spread(key = action_type, value = n, fill = 0)
ungroup() %>% 
  group_by(northstar_id, action_time) %>%
  tally

melsamp <- melsamp %>% mutate(sample = "hello")
melsamp <- melsamp %>% mutate(frequency = 2)

melsamp %>% spread(key = c("action_type", "sample"), value = c("freq", "frequency"), fill = 0)


mel_samp2 <-
  mel_samp %>%
  spread(key = )

############ Email data ############


## Preparing email data to merge with Users data (unique northstar_id in each row)

#email_ready <- 
email1 %>% 
  group_by(northstar_id, event_type) %>%
  tally %>% 
  spread(key = event_type, value = n, fill = 0) # email that's been spread to contain variables clicked, converted, opened, unsubscribed 

#email1 <- 
#email1 %>% 
#select(northstar_id, timestamp, event_type)
#range(email1$timestamp)

#email1$email_activity_hour <- hour(email1$timestamp) # is this variable necessary? 
#summary(email1$email_activity_hour)
# email_samp$email_activity_time <- 
#case_when(hour(email1$timestamp) >= 0 & hour(email1$timestamp) < 6 ~ "early AM",
#hour(email1$timestamp) >= 6 & hour(email1$timestamp) < 12 ~ "AM",
#hour(email1$timestamp) >= 12 & hour(email1$timestamp) < 18 ~ "afternoon",
#hour(email1$timestamp) >= 18 & hour(email1$timestamp) < 24 ~ "evening")

unique(phoenix$event_name)
unique





















