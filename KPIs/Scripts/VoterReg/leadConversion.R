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
  )

leads_fnames <- list.files('../../Downloads/', 'leads*', full.names=TRUE)

for (name in leads_fnames) {
  lead_source_str <-
    name %>%
    strsplit('_') %>%
    sapply("[", 2) %>%
    strsplit(split='\\.') %>%
    sapply('[', 1)

  df_sub <- read.csv(name, head=TRUE)
  df_sub$lead_source <- lead_source_str

  if('source' %in% colnames(df_sub)) {
    df_sub <- subset(df_sub, select = -c(source))
  }

  print(lead_source_str)
  if(length(df) == 0) {
    df <- df_sub
  } else {
    df <- rbind(df, df_sub)
  }
  print(nrow(df))
}

# remove dosomething members
df <- dplyr::filter(df, !grepl('*dosomething*', Email))
print(nrow(df))

# sorted by Email and Date
df <- df[with(df, order(df$Email, df$Date)),]

# remove duplicates with same lead
df_dedupe_same <- df[!duplicated(df[c('Email', 'lead_source')]),]
print(nrow(df_dedupe_same))

# remove duplicates with different leads
df_deduped <- df_dedupe_same[!duplicated(df_dedupe_same[c('Email')]),]
print(nrow(df_deduped))

# dedupe rtvvr
rtvvr <- rtvvr[!duplicated(rtvvr['nsid']),]

# merge leads with signups
df_merge <- merge(df_deduped, rtvvr, by.x = 'Email', by.y = 'email', all.x = TRUE)

# grouped df
grouped_df <-
  group_by(df_merge, lead_source) %>%
  summarise(count_confirmed = sum(!is.na(ds_vr_status)), count_total = n()) %>%
  mutate(conversion = count_confirmed / count_total)
