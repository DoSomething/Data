source('config/init.R')
source('config/pgConnect.R')
library(glue)
library(googlesheets)
pg <- pgConnect()

# Data prep ---------------------------------------------------------------
getWorkbookKey <- function(searchPhrase) {

  key <-
    gs_ls() %>%
    filter(grepl(searchPhrase,sheet_title)) %>%
    select(sheet_key) %>%
    as.character()

  return(key)

}

getWorkBook <- function(key) {

  workbook <-
    gs_key(key)

  return(workbook)

}

getSheetName <- function(key) {

  sheetNames <-
    gs_key(key) %>%
    gs_ws_ls()

  return(sheetNames)
}

getWorksheet <- function(sheetNames, Workbook, whichSheet) {

  whichSheet <- which(sheetNames==whichSheet)
  sheet <-  Workbook %>% gs_read(sheetNames[whichSheet])

  return(sheet)

}

getSheet <- function(workbook, sheet) {

  workbookID <- getWorkbookKey(workbook)
  workbook <- getWorkBook(workbookID)
  sheetNames <- getSheetName(workbookID)
  cioConv <- getWorksheet(sheetNames, workbook, sheet)

  return(cioConv)

}

getData <- function() {

  q <-
    glue_sql(
      "SELECT DISTINCT
        s.northstar_id as nsid,
        s.campaign_id::varchar,
        s.campaign_run_id::varchar,
        CASE WHEN p.status IN ('rejected','pending') THEN 'uncertain'
          ELSE p.status END AS ds_vr_status,
        CASE WHEN p.created_at < '2017-01-01'
             THEN p.created_at + interval '4 year'
             ELSE p.created_at END AS created_at,
        CASE WHEN p.source='rock-the-vote' THEN 'RockTheVote'
          ELSE 'TurboVote' END AS file,
        ref.referral_code,
        ref.email,
        u.created_at AS ds_registration_date,
        CASE WHEN u.source = 'niche' THEN 'niche'
          WHEN u.source = 'sms' THEN 'sms'
        ELSE 'web' END AS user_source
      FROM public.signups s
      LEFT JOIN public.posts p on s.id = p.signup_id
      INNER JOIN
        (
        SELECT DISTINCT
          *
        FROM
          (SELECT DISTINCT
            tv.post_id,
            tv.referral_code,
            NULL as email
          FROM rogue.turbovote tv
          UNION
          SELECT DISTINCT
            rtv.post_id,
            rtv.tracking_source AS referral_code,
            rtv.email
          FROM rogue.rock_the_vote rtv) det
        ) ref
      ON p.id::bigint = ref.post_id::bigint
      LEFT JOIN public.users u ON s.northstar_id = u.northstar_id
      WHERE p.id IS NOT NULL
      AND p.type = 'voter-reg'",
      .con='pg'
    )

  qres <-
    runQuery(q)

}

processReferralColumn <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(dat$referral_code, ',')))))+1

  parsedSep <-
    dat %>%
    select(nsid, campaign_run_id, referral_code) %>%
    mutate(
      referral_code = gsub('sourcedetails','source_details',referral_code),
      referral_code = gsub('iframe\\?r=','',referral_code),
      referral_code = gsub('iframe','',referral_code)
    ) %>%
    separate(referral_code, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
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
      source = case_when(
        source == 'sms_share' ~ 'sms',
        grepl('referral=true', referral_code) ~ 'web',
        grepl('niche', source_details) ~ 'partner',
        grepl('source',source) ~ 'no_attribution',
        source=='ema0il' ~ 'email',
        TRUE ~ source
      ),
      source_details = case_when(
        grepl('referral=true', source_details) ~ 'referral',
        TRUE ~ source_details
      ),
      newsletter = case_when(
        source == 'email' & grepl('newsletter', source_details) ~ gsub('newsletter_', '', source_details),
        TRUE ~ ''
      )
    ) %>%
    select(-A,-B,-C,-D,-E,-`F`)

  return(parsedSep)

}

addDetails <- function(dat) {

  dat %<>%
    mutate(
      details = case_when(
        source == 'social' &
          grepl('twitter', source_details) ~ 'twitter',
        source == 'social' &
          grepl('facebook', source_details) | grepl('fb',source_details) ~ 'facebook',
        source == 'social' &
          grepl('dsaboutgvp', source_details) ~ 'ds_share',
        source == 'social' &
          grepl('tumblr', source_details) ~ 'tumblr',
        source == 'social' &
          grepl('survey', source_details) ~ 'survey',
        source == 'web' &
          grepl('11_facts', source_details) ~ '11_facts',
        source == 'web' &
          grepl('hellobar', source_details) ~ 'hellobar',
        source == 'web' &
          grepl('affirmation', source_details) ~ 'affirmation',
        source == 'web' &
          grepl('VoterBlock', source_details) ~ 'voter_block',
        source == 'web' &
          grepl('campaign', source_details) ~ 'campaigns_page',
        source == 'web' &
          grepl('quiz', source_details) ~ 'quiz',
        source == 'web' &
          grepl('homepage', source_details) ~ 'homepage',
        source == 'web' &
          grepl('landingpage', source_details) ~ 'landing_page',
        source == 'web' &
          grepl('redirect', source_details) ~ 'redirect',
        source == 'web' &
          grepl('sms', source_details) ~ 'sms',
        source == 'web' &
          grepl('Social', source_details) ~ 'Social Referral',
        source == 'web' &
          grepl('typeform', source_details) ~ 'survey',
        source == 'sms' & grepl('2018', source_details) ~ 'sms_tests',
        newsletter != '' ~ newsletter,
        source_details == '' ~ 'blank',
        TRUE ~ source_details
      )
    )

  return(dat)

}

addFields <- function(dat) {

  dat %<>%
    mutate(
      reportback =
        ifelse(
          ds_vr_status %in% c('confirmed','register-form','register-OVR'), 1, 0
        ) %>% as.logical,
      month = month(created_at),
      week = case_when(
        created_at < '2018-02-06' ~ as.character('2018-01-26'),
        TRUE ~
          cut(
            as.Date(created_at),
            breaks=
              seq.Date(as.Date('2018-02-06'),as.Date('2020-01-01'),by = '7 days')
          ) %>% as.character()
      )
    )
}

prepData <- function(..., testing=F) {

  d <- getData(...)
  refParsed <- processReferralColumn(d)

  vr <-
    d %>%
    select(-referral_code) %>%
    left_join(refParsed)

  if (testing==T) {
    browser()
  }

  vr <- addDetails(vr)

  vr <- addFields(vr)

  return(vr)

}

tvrtv <- prepData()
