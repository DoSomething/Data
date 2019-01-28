library(stringr)
library(glue)
library(zip)
source('Scripts/VoterReg/rogueTVRTV.R')
source('Scripts/VoterReg/schoolTheVote.R')


library(reshape2)

vr <-
  tvrtv %>%
  bind_rows(stv) %>%
  filter(grepl('register',ds_vr_status))

rtvfile <- 'Data/RockTheVote/rock_the_vote_2019-01-17.csv'

rtv_extra <-
  suppressWarnings(read_csv(rtvfile)) %>%
  filter(Status == 'Complete') %>%
  select(
    email=`Email address`, dob=`Date of birth`,
    created_at=`Started registration`, zip=`Home zip code`
    ) %>%
  mutate(
    created_at = as.POSIXct(created_at, tz='UTC'),
    dob = as.Date(dob, '%m/%d/%Y')
    )

tvfile <- 'Data/Turbovote/testing-dosomething.turbovote.org-dosomething.turbovote.org-2019-01-25.csv'

tv_extra <-
  suppressWarnings(read_csv(tvfile)) %>%
  filter(
    !is.na(email) &
      (`voter-registration-status` == 'initiated' |
      (`voter-registration-status` == 'registered' &
       `voter-registration-method` == 'online')
      )
  ) %>%
  select(
    email, dob=dob, zip=`registered-address-zip`, created_at=`created-at`
  ) %>%
  mutate(
    dob=as.Date(paste0('1/1/',dob), '%m/%d/%Y')
    )

both_extra <-
  bind_rows(rtv_extra, tv_extra) %>%
  group_by(email) %>%
  filter(created_at==max(created_at)) %>%
  select(-created_at)

vr <-
  vr %>%
  left_join(both_extra)
