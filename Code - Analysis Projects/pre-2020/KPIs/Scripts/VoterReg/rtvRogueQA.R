pg <- pgConnect()
pg <- pgConnect(QA=T)

getRTVData <- function() {

  q <-
    glue_sql(
      "SELECT
        c.northstar_id as nsid,
        c.campaign_id::varchar,
        c.campaign_run_id::varchar,
        CASE WHEN c.post_status IN ('rejected','pending')
          THEN 'uncertain'
          ELSE c.post_status END AS ds_vr_status,
        c.post_attribution_date AS created_at,
        r.tracking_source as referral_code,
        u.created_at AS ds_registration_date,
        CASE WHEN u.source = 'niche' THEN 'niche'
          WHEN u.source = 'sms' THEN 'sms'
          ELSE 'web' END AS user_source
      FROM public.campaign_activity c
      INNER JOIN
        (
        SELECT DISTINCT *
    	  FROM rogue.rock_the_vote
        ) r ON c.post_id::bigint = r.post_id::bigint
      LEFT JOIN public.users u ON c.northstar_id = u.northstar_id
      WHERE c.post_id IS NOT NULL
      AND c.post_type = 'voter-reg'",
      .con='pg'
    )
  qres <- runQuery(q)

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
