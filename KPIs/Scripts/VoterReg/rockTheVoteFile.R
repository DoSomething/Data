library(glue)
library(digest)

getRTVFile <- function(path) {

  data <-
    suppressWarnings(suppressMessages(read_csv(path))) %>%
    filter(
      !grepl('dosomething', `Email address`) &
      !grepl('testing', `Tracking Source`) &
      !grepl('@example.com', `Email address`)
    )

  for (i in 1:length(names(data))) {
    if(grepl('-', names(data)[i])) {
      names(data)[i] <- tolower(gsub('-','_',names(data)[i]))
    } else if(grepl(' ', names(data)[i])) {
      names(data)[i] <- tolower(gsub(' ','_',names(data)[i]))
    } else {
      names(data)[i] <- tolower(names(data)[i])
    }
  }

  data %<>%
    mutate(
      id=sapply(
        paste(
          email_address,
          started_registration,
          status,
          tracking_source),
        digest,algo='md5'
        )
    ) %>%
    filter(!duplicated(id))

  ## TODO: How do we deal with duplicate emails w/ no NSID? Take the latest?
  ## Or follow hierarchy?

  return(data)

}


getWebLeads <- function() {

  raw <- tibble()

  for (i in 1:7) {
    a <- suppressMessages(read_csv(paste0('Data/WebLeads/leads-',i,'.csv')))
    raw <- bind_rows(a,raw)
  }

  leads <-
    raw %>%
    mutate(
      Date = as.POSIXct(Date, format='%m/%d/%Y %H:%M:%S'),
      tracking_source_alt =
        case_when(
          is.na(source) ~ r,
          is.na(r) ~ source,
          TRUE ~ NA_character_
        )
    ) %>%
    filter(Date >= '2018-06-26' & Date <= '2018-06-27') %>%
    group_by(Email, `Zip Code`) %>%
    filter(Date==max(Date)) %>%
    select(email_address=Email, home_zip_code=`Zip Code`, tracking_source_alt) %>%
    ungroup()

  return(leads)

}

attachWebLeadTracking <- function(rtv) {

  webLeads <- getWebLeads()

  out <-
    rtv %>%
    left_join(webLeads) %>%
    mutate(
      tracking_source =
        case_when(
          tracking_source %in% c('[r]','undefined','[source]') |
          is.na(tracking_source) ~ tracking_source_alt,
          TRUE ~ tracking_source
        )
    ) %>%
    select(-tracking_source_alt)

}

processTrackingSource <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(dat$tracking_source, ',')))))+1

  parsedSep <-
    dat %>%
    select(id, tracking_source) %>%
    mutate(
      tracking_source = gsub('sourcedetails','source_details',tracking_source),
      tracking_source = gsub('iframe\\?r=','',tracking_source),
      tracking_source = gsub('iframe','',tracking_source)
    ) %>%
    separate(tracking_source, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
      nsid =
        case_when(
          grepl('referral=true', tracking_source) ~ '',
          grepl('user',tolower(A)) ~ gsub(".*:",'',A),
          grepl('user',tolower(B)) ~ gsub(".*:",'',B),
          TRUE ~ ''
        ),
      campaign_id =
        case_when(
          grepl('campaignid',tolower(A)) ~ gsub(".*:",'',A),
          grepl('campaign:',tolower(A)) ~ gsub(".*:",'',A),
          grepl('campaignid',tolower(B)) ~ gsub(".*:",'',B),
          grepl('campaign:',tolower(B)) ~ gsub(".*:",'',B),
          TRUE ~ ''
        ),
      campaign_run_id =
        case_when(
          grepl('campaignrunid',tolower(B)) ~ gsub(".*:",'',B),
          grepl('campaignrunid',tolower(B)) ~ gsub(".*=",'',B),
          grepl('campaignrunid',tolower(C)) ~ gsub(".*:",'',C),
          grepl('campaignrunid',tolower(C)) ~ gsub(".*=",'',C),
          TRUE ~ ''
        ),
      source =
        case_when(
          grepl('ads',tolower(A)) ~ 'ads',
          grepl('email',tolower(A)) ~ 'email',
          grepl('source:',tolower(C)) ~ gsub(".*:",'',C),
          grepl('source=',tolower(C)) ~ gsub(".*=",'',C),
          grepl('source:',tolower(D)) ~ gsub(".*:",'',D),
          grepl('source=',tolower(D)) ~ gsub(".*=",'',D),
          TRUE ~ 'no_attribution'
        ),
      source_details =
        case_when(
          grepl('_details:',tolower(D)) ~ gsub(".*:",'',D),
          grepl('_details=',tolower(D)) ~ gsub(".*=",'',D),
          grepl('_details:',tolower(E)) ~ gsub(".*:",'',E),
          grepl('_details=',tolower(E)) ~ gsub(".*=",'',E),
          TRUE ~ ''
        ),
      campaign_id =
        ifelse(campaign_run_id=='' & campaign_id=='', '8017',
               ifelse(campaign_id=='' & campaign_run_id=='8022', '8017', campaign_id)),
      source = case_when(
        source == 'sms_share' ~ 'sms',
        grepl('referral=true', tracking_source) ~ 'web',
        grepl('niche', source_details) ~ 'partner',
        TRUE ~ source
        ),
      source_details = case_when(
        grepl('referral=true', source_details) ~ 'referral',
        TRUE ~ source_details
      ),
      newsletter = case_when(
        source == 'email' & grepl('newsletter', source_details) ~
          gsub('newsletter_', '', source_details),
        TRUE ~ ''
      )
    ) %>%
    select(-A,-B,-C,-D,-E)

  return(parsedSep)

}

getQuasarAttributes <-  function(queryObjects) {

  q <- glue_sql(
    "SELECT
    u.northstar_id AS nsid,
    u.created_at AS ds_registration_date,
    CASE WHEN u.source = 'niche' THEN 'niche'
         WHEN u.source = 'sms' THEN 'sms'
         ELSE 'web' END AS user_source,
    u.subscribed_member AS active_member,
    c.signup_id,
    c.campaign_run_id,
    max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) as reportedback
    FROM public.users u
    LEFT JOIN public.campaign_activity c ON c.northstar_id = u.northstar_id
    WHERE u.northstar_id IN ({nsids*})
    GROUP BY 1,2,3,4,5,6
    ",
    nsids = queryObjects,
    .con = pg
  )

  nsrDat <-
    runQuery(q) %>%
    group_by(nsid) %>%
    summarise(
      ds_registration_date = max(ds_registration_date),
      user_source = max(user_source),
      signups = n(),
      reportbacks = sum(reportedback)
    )
}

addRTVFields <- function(dat) {

  dat %<>%
    mutate(
      ds_vr_status.record =
        case_when(
          status == 'Complete' & finish_with_state=='No' ~ 'register-form',
          status == 'Complete' & finish_with_state=='Yes' ~ 'register-OVR',
          status %in% c('Rejected','Under 18') ~ 'ineligible',
          grepl('Step', status) ~ 'uncertain',
          TRUE ~ ''
        )
      ,
      reportback.record = case_when(
        ds_vr_status.record %in%
          c('confirmed','register-form','register-OVR') ~ 1,
        ds_vr_status.record %in%
          c('uncertain','ineligible') ~ 0,
        TRUE ~ NA_real_
      ),
      created_at = as.POSIXct(started_registration,tz="UTC"),
      month = month(created_at),
      week = case_when(
        created_at < '2018-02-06' ~ as.character('2018-01-26'),
        TRUE ~
          cut(
            as.Date(created_at),
            breaks=
              seq.Date(as.Date('2018-02-06'),as.Date('2019-01-01'),by = '7 days')
          ) %>% as.character()
      )
    ) %>%
    group_by(nsid) %>%
    mutate(
      ds_vr_status =
        case_when(
          nsid=='' ~ ds_vr_status.record,
          max(ds_vr_status.record=='register-form')==1 ~ 'register-form',
          max(ds_vr_status.record=='register-OVR')==1 ~ 'register-OVR',
          max(ds_vr_status.record=='ineligible')==1 ~ 'ineligible',
          max(ds_vr_status.record=='uncertain')==1 ~ 'uncertain',
          TRUE ~ ''
        ),
      reportback =
        as.logical(ifelse(nsid=='', reportback.record,
               ifelse(max(reportback.record==1), 1, 0)))
      ,
      created_at =
        as.POSIXct(ifelse(nsid=='', created_at, max(created_at)), origin='1970-01-01')
    ) %>%
    ungroup() %>%
    select(-reportback.record, -ds_vr_status.record, -started_registration)

  return(dat)

}

alignNames <- function(data) {

  data %<>%
    select(
      created_at,referral_code=tracking_source, nsid, source,
      source_details, campaign_id, campaign_run_id, newsletter, details,
      month, week, ds_vr_status, reportback, email=email_address,
      ds_registration_date, user_source
    )

  return(data)

}

prepData <- function(...) {

  d <- getRTVFile(...)
  d <- attachWebLeadTracking(d)
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
    select(-type, -category) %>%
    group_by(email) %>%
    filter(
      nsid != '' |
      grepl('register', ds_vr_status) |
      created_at == max(created_at)
    ) %>%
    ungroup() %>%
    select(-email)

  return(vr)

}

rtv <- prepData(paste0('Data/RockTheVote/rock_the_vote_',latest_file,'.csv'))
