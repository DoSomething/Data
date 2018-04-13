getNPS <- function(x, maxValue=NA) {
  # browser()
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

  a <- prepQueryObjects(nps.q3$northstar_id)

  q<- paste0("
             SELECT u.northstar_id,
             count(DISTINCT ca.signup_id) AS total_signups,
             sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
             max(case when greatest(u.last_accessed, u.last_logged_in) > u.created_at AND u.source='niche' THEN 1 ELSE 0 END) as Niche_activated
             FROM quasar.users u
             LEFT JOIN quasar.campaign_activity ca
             ON u.northstar_id=ca.northstar_id
             WHERE u.northstar_id IN", a,
             "GROUP BY u.northstar_id")
  qres <-
    runQuery(q, which='mysql')

  nps.q3 <-
    nps.q3 %>%
    left_join(qres) %>%
    mutate(
      bad_niche=ifelse(group =='niche' & Niche_activated==0 & total_signups<=1 & total_rbs < 1,1,0),
      bad_niche = ifelse(is.na(bad_niche), 0, bad_niche)
    )

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

  a <- prepQueryObjects(nps.q4$northstar_id)

  q <- paste0("
              SELECT u.northstar_id,
              count(DISTINCT ca.signup_id) AS total_signups,
              sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
              max(case when greatest(u.last_accessed, u.last_logged_in) > u.created_at AND u.source='niche' THEN 1 ELSE 0 END) as Niche_activated
              FROM quasar.users u
              LEFT JOIN quasar.campaign_activity ca
              ON u.northstar_id=ca.northstar_id
              WHERE u.northstar_id IN", a,
              "GROUP BY u.northstar_id")
  qres <-
    runQuery(q, which='mysql')

  nps.q4 <-
    nps.q4 %>%
    left_join(qres) %>%
    mutate(
      bad_niche=ifelse(group =='niche' & Niche_activated==0 & total_signups<=1 & total_rbs < 1,1,0),
      bad_niche = ifelse(is.na(bad_niche), 0, bad_niche)
    )

  return(nps.q4)

}

mungeNPSQ2 <- function() {

  nps.q2 <-
    read_csv('Q4/Data/q2_nps.csv') %>%
    setNames(c('response_id','email','nps')) %>%
    filter(!is.na(nps))

  emailLookup <- prepQueryObjects(nps.q2$email)

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

  nps.q2.nsids <- runQuery(q, which='mysql')

  nps.q2 %<>%
    left_join(nps.q2.nsids) %>%
    mutate(
      group =
        case_when(
          niche==1 ~ 'niche',
          TRUE ~ 'other'
        ),
      type = 'Q2 2017'
    ) %>%
    select(northstar_id, nps, group, type)

  a <- prepQueryObjects(nps.q2$northstar_id)

  q <- paste0("
              SELECT u.northstar_id,
              count(DISTINCT ca.signup_id) AS total_signups,
              sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
              max(case when greatest(u.last_accessed, u.last_logged_in) > u.created_at AND u.source='niche' THEN 1 ELSE 0 END) as Niche_activated
              FROM quasar.users u
              LEFT JOIN quasar.campaign_activity ca
              ON u.northstar_id=ca.northstar_id
              WHERE u.northstar_id IN", a,
              "GROUP BY u.northstar_id")
  qres <-
    runQuery(q, which='mysql')

  nps.q2 <-
    nps.q2 %>%
    left_join(qres) %>%
    mutate(
      bad_niche=ifelse(group =='niche' & Niche_activated==0 & total_signups<=1 & total_rbs < 1,1,0),
      bad_niche = ifelse(is.na(bad_niche), 0, bad_niche)
    )

  return(nps.q2)

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

plotMod <- function(field, data) {
  require(scales)

  surveyMod.form = as.formula(paste0('nps ~ ',field,' + group + ',field,':group'))
  surveyMod <- lm(surveyMod.form, data)

  fieldMod.form = as.formula(paste0('nps ~ ',field))
  fieldMod <- lm(fieldMod.form, data)

  preds <-
    as.tibble(expand.grid(group = c('other','sms_only','niche','full'),
                          predictor = seq(0,1))) %>%
    arrange(group, predictor) %>%
    setNames(c('group',field))

  preds <- data.table(preds)

  preds[group!='full',predictions := predict(surveyMod, preds[group!='full'], type='response')]
  preds[group=='full',predictions := predict(fieldMod, preds[group=='full'], type='response')]
  preds <- preds[group=='full']
  impact <- preds[2,predictions] - preds[1,predictions]

  p <-
    ggplot(preds, aes(x=get(field), y=predictions)) +
    geom_line() +
    scale_y_continuous(breaks=pretty_breaks(10)) +
    scale_x_continuous(breaks=c(0,1)) +
    labs(y="Expected NPS", x=paste(field), title=paste0("Predicted NPS for ",field))

  return(impact)

}

percentDid <- function(thisCampaign, data) {
  pct <-
    data %>%
    group_by(northstar_id) %>%
    filter(!is.na(campaign)) %>%
    mutate(
      didCampaign = max(campaign == thisCampaign)
    ) %>%
    ungroup() %>%
    summarise(percentDid = sum(didCampaign) / n())
  out <- pct %>% bind_cols(data.frame(campaign = thisCampaign))
  return(out)
}

npsDidCampaign <- function(data, field, cutoff) {
  # browser()
  filtered <-
    data %>%
    filter(!is.na(campaign) & campaign %in% field) %>%
    filter(!duplicated(northstar_id))
  niche <- filtered %>% filter(group=='niche')
  npsNiche <- getNPS(niche$nps, cutoff)
  nonniche <- filtered %>% filter(group!='niche')
  npsNonNiche <- getNPS(nonniche$nps, cutoff)
  nps <- getNPS(filtered$nps, cutoff)
  if (length(field)==1) {
    frame <- data.frame(
      campaign=field, nps=nps,
      npsNiche = npsNiche, npsNonNiche=npsNonNiche,
      countNiche = nrow(niche),
      countNonNiche = nrow(nonniche))
  } else {
    frame <- data.frame(nps=nps)
  }
  return(frame)
}
