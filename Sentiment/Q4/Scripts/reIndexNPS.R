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

breakdown <-
  nps %>%
  group_by(type, group) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    proportion = count / sum(count)
  ) %>% arrange(type, group) %>% data.table()

# nps <-
#   nps %>%
#   mutate(
#     sample_weight =
#       case_when(
#         group=='niche' ~ breakdown[group=='niche' & type=='Q4 2017',proportion],
#         group=='other' ~ breakdown[group=='other' & type=='Q4 2017',proportion],
#         group=='sms_only' ~ breakdown[group=='sms_only' & type=='Q4 2017',proportion]
#       )
#   )


# 2016 Calibration --------------------------------------------------------

nicheN <-
  breakdown[group=='niche' & type=='Q4 2017',proportion] * nrow(nps.2016)
otherN <-
  breakdown[group=='other' & type=='Q4 2017',proportion] * nrow(nps.2016)
smsN <-
  breakdown[group=='sms_only' & type=='Q4 2017',proportion] * nrow(nps.2016)

dist2016 <- c()
for (i in 1:1000) {
  nps2016Sample <-
    nps %>%
    filter(type=='2016' & group=='niche') %>%
    sample_n(size = nicheN,replace = T) %>%
    bind_rows(
      nps %>%
        filter(type=='2016' & group=='other') %>%
        sample_n(size = otherN,replace = T)
    ) %>%
    bind_rows(
      nps %>%
        filter(type=='2016' & group=='sms_only') %>%
        sample_n(size = smsN,replace = T)
    )

  nps2016Sample %>% group_by(group) %>% summarise(count=n()) %>% mutate(proportion=count/sum(count))

  x <- getNPS(nps2016Sample$nps, 11)
  dist2016 <- c(x,dist2016)
}
ggplot(data.frame(dist2016), aes(x=dist2016)) +
  geom_density() +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  ggtitle('Recalibrated NPS Distribution - 2016')


# Calibrate Q3 ------------------------------------------------------------

nicheN <-
  breakdown[group=='niche' & type=='Q4 2017',proportion] * nrow(nps.q3)
otherN <-
  breakdown[group=='other' & type=='Q4 2017',proportion] * nrow(nps.q3)
smsN <-
  breakdown[group=='sms_only' & type=='Q4 2017',proportion] * nrow(nps.q3)

dist2017.q3 <- c()
for (i in 1:1000) {
  nps2016Sample <-
    nps %>%
    filter(type=='Q3 2017' & group=='niche') %>%
    sample_n(size = nicheN,replace = T) %>%
    bind_rows(
      nps %>%
        filter(type=='Q3 2017' & group=='other') %>%
        sample_n(size = otherN,replace = T)
    ) %>%
    bind_rows(
      nps %>%
        filter(type=='Q3 2017' & group=='sms_only') %>%
        sample_n(size = smsN,replace = T)
    )

  nps2016Sample %>% group_by(group) %>% summarise(count=n()) %>% mutate(proportion=count/sum(count))

  x <- getNPS(nps2016Sample$nps, 11)
  dist2017.q3 <- c(x,dist2017.q3)
}
ggplot(data.frame(dist2017.q3), aes(x=dist2017.q3)) +
  geom_density() +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  ggtitle('Recalibrated NPS Distribution - 2017 Q3')
