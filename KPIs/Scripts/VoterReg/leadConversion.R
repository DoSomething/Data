library(stringr)
library(openxlsx)
library(dplyr)
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

# group by lead source to calculate conversion
grouped_df <-
  group_by(df, lead_source) %>%
  summarise(count_confirmed = sum(!is.na(ds_vr_status)), count_total = n()) %>%
  mutate(conversion = count_confirmed / count_total)
