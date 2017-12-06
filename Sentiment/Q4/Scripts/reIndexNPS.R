source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

getNPS <- function(x, maxValue=NA) {
  if (!(maxValue %in% c(10,11)) | is.na(maxValue)) {
    stop('Please provide a maximum NPS value of either 10 or 11')
  }
  if (maxValue==11) {
    promotorLimit <- 10
    detractorLimit <- 7
  } else {
    promotorLimit <- 9
    detractorLimit <- 6
  }
  promoters <- length(which(x>=promotorLimit))/length(x)
  detractors <- length(which(x<=detractorLimit))/length(x)
  nps <- round((promoters - detractors)*100)
  return(nps)
}

mungeNPSQ3 <- function() {

  nps.q3 <-
    read_csv('Q4/Data/q3_nps.csv') %>%
    mutate(
      group =
        case_when(
          survey=='Niche' ~ 'niche',
          grepl('SMS', survey) ~ 'sms_only',
          TRUE ~ 'other'
        ),
      timePeriod =
        case_when(
          grepl('12', survey) ~ '12',
          grepl('3', survey) ~ '3',
          TRUE ~ 'NA'
        ),
      type = 'Q3 2017'
    ) %>%
    select(northstar_id, nps, group, timePeriod, type)

  return(nps.q3)

}

mungeNPSQ4 <- function() {

  nps.q4 <-
    read_csv('Q4/Data/q4_nps.csv') %>%
    mutate(
      nps = nps+1,
      group =
        case_when(
          survey=='niche' ~ 'niche',
          survey=='sms' ~ 'sms_only',
          TRUE ~ 'other'
        ),
      type = 'Q4 2017'
    ) %>%
    select(northstar_id, nps, group, type)
  return(nps.q4)

}

mungeNPS2016 <- function() {

  nps.2016.web <-
    read_csv('Q4/Data/nps_2016.csv') %>%
    setNames(c('response_id','email','nps')) %>%
    filter(!is.na(nps))

  nps.2016.sms <-
    read_csv('Q4/Data/nps_2016_sms.csv') %>%
    setNames(c('response_id','email','nps')) %>%
    filter(!is.na(nps))

  nps.2016 <-
    nps.2016.web %>%
    bind_rows(nps.2016.sms)

  emailLookup <- prepQueryObjects(nps.2016$email)

  q <-
    paste0(
      "SELECT
      u.northstar_id,
      CASE WHEN u.source = 'niche'
        THEN 1 ELSE 0 END AS niche,
      CASE WHEN (u.customer_io_subscription_status IS NULL
                OR u.customer_io_subscription_status = 'subscribed')
            AND (u.email IS NULL OR LENGTH(u.email) = 0)
            AND u.mobile IS NOT NULL
            AND u.sms_status = 'active'
        THEN 1 ELSE 0 END AS sms_only,
      u.email
    FROM quasar.users u
    WHERE u.email in ", emailLookup
    )

  nps2016.nsids <- runQuery(q, which='mysql')

  nps.2016 %<>%
    left_join(nps2016.nsids) %>%
    mutate(
      group =
        case_when(
          niche==1 ~ 'niche',
          sms_only==1 | response_id %in% nps.2016.sms$response_id ~ 'sms_only',
          TRUE ~ 'other'
        ),
      type = '2016'
      ) %>%
    select(northstar_id, nps, group, type)

    return(nps.2016)
}

nps.q3 <- mungeNPSQ3()
nps.q4 <- mungeNPSQ4()
nps.2016 <- mungeNPS2016()

nps <-
  nps.q3 %>%
  bind_rows(nps.q4) %>%
  bind_rows(nps.2016)

ggplot(nps, aes(x=nps, color=type)) +
  geom_density() + facet_grid(~group) +
  scale_x_continuous(breaks=pretty_breaks(10))
