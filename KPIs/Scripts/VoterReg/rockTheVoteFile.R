library(glue)
library(digest)

getRTVFile <- function(path) {

  data <-
    suppressWarnings(suppressMessages(read_csv(path))) %>%
    filter(
      !grepl('dosomething', `Email address`) &
      !grepl('testing', `Tracking Source`) &
      !grepl('lkpttn@gmail.com', `Email address`) &
      !grepl('@example.com', `Email address`) &
      !grepl('david@dfurnes.com', `Email address`) &
      !grepl('ashleyebaldwin@gmail.com', `Email address`)
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

processTrackingSource <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(dat$tracking_source, ',')))))+1

  parsedSep <-
    dat %>%
    select(id, tracking_source) %>%
    mutate(
      tracking_source = gsub('iframe\\?r=','',tracking_source),
      tracking_source = gsub('iframe','',tracking_source)
    ) %>%
    separate(tracking_source, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
      nsid =
        case_when(
          grepl('user',tolower(A)) ~ gsub(".*:",'',A),
          grepl('user',tolower(B)) ~ gsub(".*:",'',B),
          TRUE ~ ''
        ),
      campaignId =
        case_when(
          grepl('campaignid',tolower(A)) ~ gsub(".*:",'',A),
          grepl('campaignid',tolower(B)) ~ gsub(".*:",'',B),
          TRUE ~ ''
        ),
      campaignRunId =
        case_when(
          grepl('campaignrunid',tolower(B)) ~ gsub(".*:",'',B),
          grepl('campaignrunid',tolower(C)) ~ gsub(".*:",'',C),
          TRUE ~ ''
        ),
      source =
        case_when(
          grepl('source:',tolower(C)) ~ gsub(".*:",'',C),
          grepl('source:',tolower(D)) ~ gsub(".*:",'',D),
          TRUE ~ 'no_attribution'
        ),
      source_details =
        case_when(
          grepl('_details:',tolower(D)) ~ gsub(".*:",'',D),
          grepl('_details:',tolower(E)) ~ gsub(".*:",'',E),
          TRUE ~ ''
        ),
      campaignId =
        ifelse(campaignRunId=='' & campaignId=='', '8017',
               ifelse(campaignId=='' & campaignRunId=='8022', '8017', campaignId)),
      newsletter = case_when(
        source == 'email' & grepl('newsletter', source_details) ~
          gsub('newsletter_', '', source_details),
        TRUE ~ ''
      ),
      details = case_when(
        newsletter != '' & !is.na(newsletter) ~ newsletter,
        source == 'social' &
          grepl('twitter', source_details) ~ 'twitter',
        source == 'social' &
          grepl('facebook', source_details) | grepl('fb',source_details) ~ 'facebook',
        source == 'social' &
          grepl('dsaboutgvp', source_details) ~ 'ds_share',
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
    ) %>%
    select(-A,-B,-C,-D,-E)

  return(parsedSep)

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
      reportback.record = ifelse(
        ds_vr_status.record %in%
          c('confirmed','register-form','register-OVR'), T, F
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
        ifelse(nsid=='', reportback.record,
               ifelse(max(reportback.record==T), T, F))
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
      id, first_name, middle_name, last_name, phone, email=email_address,
      registered_address_street=home_address, registered_address_street_2=home_unit,
      registered_address_city=home_city, registered_address_zip=home_zip_code,
      registered_address_state=home_state, mailing_address_street=mailing_address,
      mailing_address_street_2=mailing_unit, mailing_address_city=mailing_city,
      mailing_address_state=mailing_state, mailing_address_zip=mailing_zip_code,
      dob=date_of_birth, language_preference=language, created_at,
      referral_code=tracking_source, nsid, source, source_details,
      campaignId, campaignRunId, newsletter, details, month, week,
      ds_vr_status, reportback
    ) %>%
    mutate(
      dob = as.integer(substr(dob, nchar(dob)-4, nchar(dob)))
    )

  return(data)

}

prepData <- function(...) {

  d <- getRTVFile(...)
  refParsed <- processTrackingSource(d)

  vr <-
    d %>%
    select(-tracking_source) %>%
    left_join(refParsed)

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
    ungroup()

  return(vr)

}

rtv <- prepData('Data/RockTheVote/rock_the_vote_05312018.csv')
