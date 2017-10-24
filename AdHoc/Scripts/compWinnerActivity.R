# https://trello.com/c/Fj5JCcNh/1141-data-request-users-who-have-won-competitions-and-scholarships
source('config/init.R')
source('config/mySQLConfig.R')
library(broom)
library(stringr)
library(dbplyr)
library(scales)

# Winners -----------------------------------------------------------------

winners <- 
  read_csv('Data/competition_winners.csv') %>% 
  filter(!is.na(Email)) %>% 
  transmute(
    email = Email,
    challenge = `Place/Competition Name`,
    enter_date = as.Date(`DATE ENTERED`, '%m/%d/%y'),
    win_date = as.Date(word(Status, -1),'%m/%d/%y'),
    campaign_id = `Campaign ID`,
    campaign_run_id = `Run ID`,
    type = 'Competition'
  ) %>% 
  mutate(
    win_date = if_else(is.na(win_date), enter_date + 7, win_date)
  ) %>% 
  bind_rows(
    read_csv('Data/scholarship_winners.csv') %>% 
      transmute(
        email = Email, 
        challenge = Scholarship,
        win_date = as.Date(Date, '%m/%d/%y'),
        campaign_run_id = `Run ID`,
        campaign_id = `Campaign ID`,
        type = 'Scholarship'
      )
  )

# winnerLookup <- hash(winners$email, winners$enter_date)

# Gladiator ---------------------------------------------------------------

qres <- runQuery('Scripts/compWinnerActivity.sql')

# Campaign Info ---------------------------------------------------

campaignInfo <- 
  tbl(con, "campaign_info") %>% 
  mutate(start_date=campaign_run_start_date, end_date=campaign_run_end_date) %>% 
  select(campaign_run_id, start_date, end_date) %>% 
  filter(!is.na(end_date) & end_date >= '2016-09-01') %>%
  collect() %>% 
  distinct() %>% 
  mutate(
    start_date = as.POSIXct(start_date),
    end_date = as.POSIXct(end_date)
  ) %>% 
  inner_join(
    qres %>% 
      group_by(campaign_run_id) %>% 
      summarise(
        hasCompetition = max(competitor)
      )
  ) %>% 
  group_by(campaign_run_id) %>% 
  filter(end_date == max(end_date) & start_date==max(start_date))

# Create analytical set ---------------------------------------------------

activity <-
  qres %>% 
  mutate(signup_created_at = as.POSIXct(signup_created_at)) %>% 
  left_join(
    winners %>% select(win_date, campaign_run_id, email, type)
    )  %>% 
  mutate(
    winner = if_else(!is.na(win_date), 1, 0),
    competitor = ifelse(winner==1 & type=='Competition', 1, competitor),
    winType = if_else(winner==1 & type == 'Competition', 'Competition', 
                      if_else(winner==1 & type=='Scholarship', 'Scholarship', 'Neither'))
  ) %>% 
  inner_join(campaignInfo) %>% 
  mutate(
    signup_created_at = if_else(signup_created_at > end_date, end_date, signup_created_at)
  ) %>% 
  arrange(northstar_id, signup_created_at) %>% 
  group_by(northstar_id) %>% 
  mutate(
    campaignCounter = 1:n(),
    nextCampaignTS = lead(signup_created_at),
    timeToNextCampaign = difftime(nextCampaignTS, signup_created_at, units='hours'),
    didAnotherCampaign = if_else(is.na(nextCampaignTS), 0, 1)
  )

# Summarise ---------------------------------------------------------------

activitySum <-
  activity %>% 
  group_by(competitor, winner, winType) %>% 
  summarise(
    People = n(),
    timeToNextCampaign = mean(timeToNextCampaign, na.rm=T),
    didAnotherCampaign = percent(mean(didAnotherCampaign)),
    reportbackRate = percent(mean(reportedback))
  )

saveCSV(activitySum, desktop=T)
