pg <- pgConnect()
pg <- pgConnect(QA=T)

getRTVData <- function() {

  q <- "SELECT * FROM rogue.rock_the_vote"
  qres <- runQuery(q, QA=T)

  qres %<>%
    mutate(
      id=sapply(
        paste(
          post_id,
          started_registration,
          status,
          tracking_source),
        digest,algo='md5'
      )
    ) %>%
    filter(!duplicated(id))

}

alignNames <- function(data) {

  data %<>%
    select(
      created_at,referral_code=tracking_source, nsid, source,
      source_details, campaign_id, campaign_run_id, newsletter, details,
      month, week, ds_vr_status, reportback,
      ds_registration_date, user_source
    )

  return(data)

}

prepRTVData <- function(testing=F) {

  d <- getRTVData()
  # d <- attachWebLeadTracking(d)
  refParsed <- processTrackingSource(d)

  vr <-
    d %>%
    select(-tracking_source) %>%
    left_join(refParsed)

  vr <- addDetails(vr)

  nsids <-
    vr %>%
    filter(nsid != '') %$%
    nsid

  nsrDat <- getQuasarAttributes(nsids)

  vr %<>%
    left_join(nsrDat)

  vr <- addRTVFields(vr)

  cioConv <-
    getSheet('Voter Registration Source Details Buckets', 'Conversions') %>%
    select(source_details, type, category) %>%
    rename(newsletter = source_details) %>%
    filter(type=='email')

  if (testing==T) {
    browser()
  }

  vr <-
    alignNames(vr) %>%
    left_join(cioConv) %>%
    mutate(
      details = case_when(
        newsletter != '' & !is.na(newsletter) ~ category,
        grepl('referral=true', referral_code) ~ 'referral',
        TRUE ~ details
      ),
      file = 'RockTheVote'
    ) %>%
    select(-type, -category)

  return(vr)

}

rtv.test <- prepRTVData()
