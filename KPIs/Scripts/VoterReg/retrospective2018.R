library(stringr)
library(glue)
source('Scripts/VoterReg/rogueTVRTV.R')
source('Scripts/VoterReg/schoolTheVote.R')
library(reshape2)
library(eeptools)
library(zipcode)
library(scales)
library(kableExtra)

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

mamPostReg <-
  set %>%
  select(nsid, created_at, neighborhood, source, age, Type) %>%
  left_join(mamActions) %>%
  mutate(
    action_buckets =
      case_when(
        action_type %in% c('site_login','site_access') ~ 'visit_website',
        action_type %in% c('messaged_gambit','bertly_link_click','bertly_link_uncertain') ~ 'sms_interaction',
        action_type %in% c('clicked_link') ~ 'email_interaction',
        action_type %in% c('signup','post') ~ 'signup/reportback',
        TRUE ~ action_type
      ),
    daysSince = as.Date(action_ts) - as.Date(created_at),
    Type.NewBreak =
      case_when(
        Type=='New' & source %in% c('source','no_attribution') ~ 'New - Ads',
        Type=='New' & !source %in% c('source','no_attribution') ~ 'New - Other',
        TRUE ~ Type
      )
  ) %>%
  filter(
    source!='on_the_ground' &
      !(Type=='Existing' & action_buckets=='account_creation' & daysSince > 0) &
      !(Type=='New' & daysSince < 0)
  )

pre2019 <- rtv_extra %>% filter(created_at < '2019-01-01')

toMatch <- c("dosomething", "@example.com", "@test.com")
testRegistrations <- length(grep(paste(toMatch,collapse="|"),
                                 pre2019$email, value=TRUE))


purgeBroadcastList <-
  c(
    'newsletter_1385',
    'houston',
    'newsletter_991button',
    'newsletter_991link',
    'broadcastID_3brtwet8H6kA4CwCMGeeIO',
    'broadcastID_7btGiJbYeQc6gqsigSSsiI',
    'broadcastID_48fjF3NjQkW80yyckAWawG'
  )

purgeRelatedDupes <-
  vr %>%
  filter(source %in% c('sms','email')) %>%
  mutate(
    purgeBroadcast =
      case_when(source_details %in% purgeBroadcastList ~ 'purge_broadcast',
                TRUE ~ 'non_purge_broadcast')
  ) %>%
  count(purgeBroadcast)
