library(stringr)
library(glue)
source('Scripts/VoterReg/rogueTVRTV.R')
source('Scripts/VoterReg/schoolTheVote.R')
library(reshape2)
library(eeptools)


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
  select(-created_at) %>%
  mutate(zip = substr(zip, 1, 5))

zipPop <-
  read_csv('Data/Zipcode-ZCTA-Population-Density-And-Area-Unsorted.csv') %>%
  setNames(c('zip','population','sq_miles','pop_density'))

set <-
  vr %>%
  left_join(both_extra) %>%
  left_join(zipPop) %>%
  mutate(
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
      )
  )

# Analysis ----------------------------------------------------------------

ggplot(filter(set, inInterval(age, c(16,75))), aes(x=neighborhood, y=age)) +
  geom_violin(aes(fill=neighborhood)) +
  scale_y_continuous(breaks=pretty_breaks(20)) +
  coord_flip()

sourceDetails <-
  set %>%
  count(source)

ggplot(sourceDetails, aes(x='', y=n, fill=source)) +
  geom_bar(position='stack', stat='identity') +
  coord_polar("y", start=0) +
  geom_text(aes(label = n), position=position_stack(vjust=.5),size=2.5) +
  theme_void()

locationSource <-
  set %>%
  filter(!is.na(neighborhood)) %>%
  count(neighborhood, source) %>%
  group_by(neighborhood) %>%
  mutate(p=round(n/sum(n),4)*100)

ggplot(locationSource, aes(x=neighborhood, y=p, fill=source)) +
  geom_bar(position='stack', stat='identity') +
  geom_text(aes(label = p), position=position_stack(vjust=.5),size=2.5)
