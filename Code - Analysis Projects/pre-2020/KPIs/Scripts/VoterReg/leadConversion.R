library(stringr)
library(openxlsx)
library(dplyr)
library(eeptools)
source('Scripts/VoterReg/rogueTVRTV.R')

library(reshape2)

rtvvr <-
  tvrtv %>%
  mutate(
    year = year(created_at)
  ) %>%
  filter (
    file == 'RockTheVote',
    ds_vr_status %in% c('register-form', 'register-OVR')
  ) %>%
  distinct(., nsid, .keep_all = TRUE)

# pull in leads csv files
leads_fnames <- list.files('../../Downloads/', 'leads*', full.names = TRUE)

for (name in leads_fnames) {
  df_sub <- read.csv(name, head = TRUE)

  # not all files have a source column; this removes that column if it does exist
  # in order to be able to concat the dataframes + it's not necessary for this effort
  if('source' %in% colnames(df_sub)) {
    df_sub <- subset(df_sub, select = -c(source))
  }

  # add a lead source column based on the filename (e.g. Snapchat, Facebook etc.)
  lead_source_str <-
    name %>%
    strsplit('_') %>%
    sapply("[", 2) %>%
    strsplit(split='\\.') %>%
    sapply('[', 1)

  df_sub$lead_source <- lead_source_str

  print(lead_source_str)

  # concat files
  if(length(df) == 0) {
    df <- df_sub
  } else {
    df <- rbind(df, df_sub)
  }
  print(nrow(df))
}

# remove dosomething members, sort by Email and Date, remove duplicate emails
# because we don't care which source it came from, left join rtv data
df <-
  filter(df, !grepl('*dosomething*', Email)) %>%
  arrange(Email, Date) %>%
  distinct(., Email, .keep_all = TRUE) %>%
  left_join(., rtvvr, by=c('Email' = 'email'))

df <- filter(df, !grepl('*test*', Email))
df <- filter(df, !grepl('g@g.com', Email))

# group by lead source to calculate conversion
grouped_df <-
  group_by(df, lead_source) %>%
  summarise(count_confirmed = sum(!is.na(ds_vr_status)), count_total = n()) %>%
  mutate(conversion = count_confirmed / count_total)


# pull in age column to look at conversion by age group using RTV file
rtvfile <- 'Data/rock_the_vote_2019-01-17.csv'

rtv_extra <-
  suppressWarnings(read_csv(rtvfile)) %>%
  filter(Status == 'Complete') %>%
  select(
    email=`Email address`, dob=`Date of birth`
  ) %>%
  mutate(
    dob = as.Date(dob, '%m/%d/%Y')
  ) %>%
  distinct(., email, .keep_all = TRUE)

df <-
  left_join(df, rtv_extra, by = c('Email' = 'email')) %>%
  filter(!is.na(dob)) %>%
  mutate(
    created_at = as.Date(as.POSIXct(created_at, 'GMT'))
  )

df <-
  filter(df, df$dob < df$created_at)

df <-
  mutate(
    df,
    age = floor(age_calc(df$dob, df$created_at, 'years'))
  )

df <-
  mutate(
    df,
    age_group = cut(
    df$age,
    breaks=c(0, 13, 18, 25, 35, 45, 55, 65, Inf),
    right = FALSE,
    labels=c('< 13', '13-17', '18-24', '25-34', '35-44', '45-54', '55-64', '65+'))
  )

df_byage <-
  df %>%
  group_by(age_group) %>%
  summarise(count = n())

df_bychannel_byage <-
  df %>%
  group_by(lead_source, age_group) %>%
  summarise(count = n())
