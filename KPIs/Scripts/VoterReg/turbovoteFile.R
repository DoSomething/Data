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
      "SELECT
        c.northstar_id as nsid,
        c.campaign_id::varchar,
        c.campaign_run_id::varchar,
        CASE WHEN c.post_status IN ('rejected','pending') THEN 'uncertain'
             ELSE c.post_status END AS ds_vr_status,
        c.post_attribution_date AS created_at,
        t.referral_code,
        u.created_at AS ds_registration_date,
        CASE WHEN u.source = 'niche' THEN 'niche'
             WHEN u.source = 'sms' THEN 'sms'
             ELSE 'web' END AS user_source
        FROM public.campaign_activity c
        LEFT JOIN rogue.turbovote t ON c.post_id::bigint = t.post_id::bigint
        LEFT JOIN public.users u ON c.northstar_id = u.northstar_id
        WHERE c.post_id IS NOT NULL
        AND c.post_type = 'voter-reg'",
      .con='pg'
    )

  qres <-
    runQuery(q,'pg') %>%
    filter(!duplicated(paste0(nsid,campaign_run_id)))
}

processReferralColumn <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(dat$referral_code, ',')))))+1
  parsedSep <-
    dat %>%
    select(nsid, campaign_run_id, referral_code) %>%
    separate(referral_code, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
      source_details =
        case_when(
          grepl('11_facts',A) ~ '11_facts',
          grepl('face',A) ~ 'facebook',
          grepl('sms',A) ~ 'sms',
          grepl('twitter',A) ~ 'twitter',
          !substr(A, 1, 4) %in% c('user','camp') ~ A,
          grepl('source_details:', D) ~ D,
          grepl('source_details:', E) ~ E,
          TRUE ~ ''
        ),
      source_details = gsub('source_details:', '', source_details),
      source =
        case_when(
          grepl('source:', B) ~ B,
          grepl('source:', C) ~ C,
          grepl('source:', D) ~ D,
          TRUE ~ ''
        ),
      source = sapply(strsplit(source, '\\:'), "[", 2),
      content =
        case_when(
          grepl('content', E) ~ gsub('utm_content:','',E),
          TRUE ~ ''
        ),
      source = case_when(
        source == 'sms_share' ~ 'sms',
        grepl('niche', source_details) ~ 'partner',
        source_details %in% c('twitter','facebook') ~ 'social',
        source_details == '11_facts' | source == 'dosomething' ~ 'web',
        is.na(source) | source=='{source}' ~ 'no_attribution',
        TRUE ~ source
      ),
      source_details = case_when(
        is.na(source_details) ~ '',
        source == 'web' & source_details == '' ~ paste0('campaign_',campaign_run_id),
        TRUE ~ source_details
      ),
      newsletter = case_when(
        source == 'email' & grepl('newsletter', source_details) ~ gsub('newsletter_', '', source_details),
        TRUE ~ ''
      )
    ) %>%
    select(-A,-B,-C,-D,-E)
  return(parsedSep)
}

addDetails <- function(dat) {

  dat %<>%
    mutate(
      details = case_when(
        newsletter != '' & !is.na(newsletter) ~ newsletter,
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
        is.na(source_details) | source_details == '' ~ 'blank',
        TRUE ~ source_details
      )
    )

  return(dat)

}

addFields <- function(dat) {
  dat %<>%
    mutate(
      reportback = ifelse(
        ds_vr_status %in%
          c('confirmed','register-form','register-OVR'), T, F
      ),
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
    )
}

prepData <- function(...) {

  d <- getData(...)
  refParsed <- processReferralColumn(d)

  vr <-
    d %>%
    select(-referral_code) %>%
    left_join(refParsed)

  vr <- addDetails(vr)

  vr <- addFields(vr)

  cioConv <-
    getSheet('Voter Registration Source Details Buckets', 'Conversions') %>%
    select(source_details, type, category) %>%
    rename(newsletter = source_details) %>%
    filter(type=='email')

  vr %<>%
    left_join(cioConv) %>%
    mutate(
      details = case_when(
        newsletter != '' & !is.na(newsletter) ~ category,
        TRUE ~ details
      ),
      file = 'TurboVote'
    ) %>%
    select(-type, -category)

  return(vr)

}

tv <- prepData()
