library(stringr)
library(glue)
source('Scripts/VoterReg/rogueTVRTV.R')
source('Scripts/VoterReg/schoolTheVote.R')
library(reshape2)
library(eeptools)
library(zipcode)
library(scales)

# Data Prep ---------------------------------------------------------------

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
  mutate(zip = substr(zip, 1, 5))

zipPop <-
  read_csv('Data/Zipcode-ZCTA-Population-Density-And-Area-Unsorted.csv') %>%
  setNames(c('zip','population','sq_miles','pop_density'))

data("zipcode")

set <-
  vr %>%
  rename(created_at_backup=created_at) %>%
  left_join(both_extra, by = 'email') %>%
  left_join(zipPop) %>%
  left_join(zipcode) %>%
  mutate(
    created_at = coalesce(created_at, created_at_backup),
    neighborhood =
      case_when(
        pop_density >= 3000 ~ 'Urban',
        pop_density >= 1000 ~ 'Suburban',
        pop_density < 1000 ~ 'Rural',
        TRUE ~ NA_character_
      ),
    age =
      case_when(
        !is.na(dob) ~
          age_calc(
            pmin(dob, as.Date(created_at), na.rm=T),
            enddate = as.Date(created_at),
            units='years'
            ),
        TRUE ~ NA_real_
      ),
    Type = case_when(user_source=='importer-app' ~ 'New', TRUE ~ 'Existing')
  )

nsids <- unique(set$nsid)

q <-
  glue_sql(
    "SELECT
      m.northstar_id AS nsid,
      m.action_type,
      m.timestamp AS action_ts
    FROM member_event_log m
    INNER JOIN (
      SELECT DISTINCT p.northstar_id
      FROM posts p
      WHERE p.type = 'voter-reg'
      AND p.status ilike '%register%'
    ) reg ON reg.northstar_id = m.northstar_id
    WHERE m.timestamp >= '2018-01-01'
    ",
    .con=pg
  )

mamActions <-
  runQuery(q)
