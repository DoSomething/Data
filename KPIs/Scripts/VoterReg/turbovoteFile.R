source('config/init.R')
source('config/mySQLConfig.R')
source('config/pgConnect.R')
library(googlesheets)
pg <- pgConnect()

latest_file <- '2018-04-30'
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

getData <- function(path) {

  vr <-
    suppressWarnings(suppressMessages(read_csv(path))) %>%
    filter(
      !grepl('thing.org', email) &
        !grepl('testing', hostname) &
        !grepl('@dosom', email) &
        !grepl('Baloney', `last-name`) &
        !grepl('turbovote', email)
    )

  for (i in 1:length(names(vr))) {
    if(grepl('-', names(vr)[i])) {
      names(vr)[i] <- gsub('-','_',names(vr)[i])
    } else if(grepl(' ', names(vr)[i])) {
      names(vr)[i] <- gsub(' ','_',names(vr)[i])
    }
  }
  return(vr)
}

processReferralColumn <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(dat$referral_code, ',')))))+1
  parsedSep <-
    dat %>%
    select(id, referral_code) %>%
    separate(referral_code, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
      nsid =
        case_when(
          substr(A, 1, 4)=='user' ~ substr(A, 6, nchar(A)),
          TRUE ~ ''
        ),
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
      campaignId =
        case_when(
          grepl('campaignID',A) ~ A,
          grepl('campaignid', tolower(B)) ~ B,
          grepl('campaign:', D) ~ D,
          grepl('campaignID:', D) ~ D,
          TRUE ~ ''
        ),
      campaignId = sapply(strsplit(campaignId, '\\:'), "[", 2),
      campaignRunId =
        case_when(
          grepl('campaignrun', tolower(C)) ~ C,
          grepl('campaign:', tolower(B)) ~ B,
          grepl('campaignrun', tolower(B)) ~ B,
          TRUE ~ ''
        ),
      campaignRunId = sapply(strsplit(campaignRunId, '\\:'), "[", 2),
      campaignId = ifelse(is.na(campaignRunId) & is.na(campaignId), '8017',
                          ifelse(is.na(campaignId) & campaignRunId=='8022', '8017', campaignId)),
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
        source_details %in% c('twitter','facebook') ~ 'social',
        source_details == '11_facts' | source == 'dosomething' ~ 'web',
        is.na(source) ~ 'no_attribution',
        TRUE ~ source
      ),
      source_details = case_when(
        is.na(source_details) ~ '',
        source == 'web' & source_details == '' ~ paste0('campaign_',campaignRunId),
        TRUE ~ source_details
      ),
      newsletter = case_when(
        source == 'email' & grepl('newsletter', source_details) ~ gsub('newsletter_', '', source_details),
        TRUE ~ ''
      ),
      details = case_when(
        newsletter != '' & !is.na(newsletter) ~ newsletter,
        TRUE ~ source_details
      )
    ) %>%
    select(-A,-B,-C,-D,-E)
  return(parsedSep)
}

getQuasarAttributes <- function(queryObjects) {
  q <- paste0(
    "SELECT
      u.northstar_id AS nsid,
      u.created_at AS ds_registration_date,
      u.source AS user_source,
      CASE WHEN u.customer_io_subscription_status = 'subscribed' OR
        u.sms_status = 'active' THEN 1 ELSE 0 END AS active_member,
      c.signup_id,
      c.campaign_run_id,
      max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) as reportedback
    FROM quasar.users u
    LEFT JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
    WHERE u.northstar_id IN ",queryObjects,"
    GROUP BY u.northstar_id, c.signup_id
    "
  )

  nsrDat <-
    runQuery(q, 'mysql') %>%
    group_by(nsid) %>%
    summarise(
      ds_registration_date = max(ds_registration_date),
      user_source = max(user_source),
      signups = n(),
      reportbacks = sum(reportedback)
    ) %>%
    mutate(
      user_source =
        case_when(
          user_source == 'niche' ~ 'niche',
          user_source == 'sms' ~ 'sms',
          TRUE ~ 'web'
        )
    )
}

addFields <- function(dat) {
  dat %<>%
    mutate(
      ds_vr_status.record =
        case_when(
          voter_registration_status == 'initiated' ~
            'register-form',
          voter_registration_status == 'registered' & voter_registration_method == 'online' ~
            'register-OVR',
          voter_registration_status %in% c('unknown','pending','') | is.na(voter_registration_status) ~
            'uncertain',
          voter_registration_status %in% c('ineligible','not-required') ~
            'ineligible',
          voter_registration_status == 'registered' ~
            'confirmed',
          TRUE ~ ''
        ),
      reportback.record = ifelse(
        ds_vr_status.record %in%
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
    ) %>%
    group_by(nsid) %>%
    mutate(
      ds_vr_status =
        case_when(
          nsid=='' ~ ds_vr_status.record,
          max(ds_vr_status.record=='register-form')==1 ~ 'register-form',
          max(ds_vr_status.record=='register-OVR')==1 ~ 'register-OVR',
          max(ds_vr_status.record=='confirmed')==1 ~ 'confirmed',
          max(ds_vr_status.record=='ineligible')==1 ~ 'ineligible',
          max(ds_vr_status.record=='uncertain')==1 ~ 'uncertain',
          TRUE ~ ''
        ),
      reportback =
        ifelse(nsid=='', reportback.record,
               ifelse(max(reportback.record==T), T, F))
      ,
      updated_at = as.POSIXct(ifelse(
        nsid=='', updated_at, max(updated_at)
        ), origin = '1970-01-01'),
      created_at = as.POSIXct(ifelse(
        nsid=='', created_at, max(created_at)
      ), origin = '1970-01-01')
    ) %>%
    ungroup() %>%
    select(-reportback.record, -ds_vr_status.record)
}

prepData <- function(...) {

  d <- getData(...)
  refParsed <- processReferralColumn(d)

  vr <-
    d %>%
    select(-referral_code) %>%
    left_join(refParsed) %>%
    mutate(
      nsid = case_when(
        nsid %in% c('5a84b01ea0bfad5dc71768a2','null') | is.na(nsid) ~ '',
        TRUE ~ nsid
        )
    )

  dupes <-
    vr %>%
    filter((duplicated(nsid) | duplicated(nsid, fromLast=T)) &
             !(nsid %in% c('','null'))) %>%
    arrange(nsid, created_at, updated_at) %>%
    mutate(
      nsidInd = cumsum(nsid != lag(nsid, default=""))
    ) %>%
    group_by(nsid) %>%
    mutate(
      recordCounter = 1:n()
    )

  nsids <-
    vr %>%
    filter(nsid != '') %$%
    nsid %>%
    prepQueryObjects()

  nsrDat <- getQuasarAttributes(nsids)

  vr %<>%
    left_join(nsrDat)

  vr <- addFields(vr)

  dupes <- addFields(dupes)

  cioConv <-
    getSheet('Voter Registration Pacing', 'Conversions') %>%
    select(source_details, type, category) %>%
    rename(newsletter = source_details) %>%
    filter(type=='email')

  vr %<>%
    filter(
      !duplicated(nsid) |
      nsid %in% c('','null')
    ) %>%
    left_join(cioConv) %>%
    mutate(
      details = case_when(
        newsletter != '' & !is.na(newsletter) ~ category,
        TRUE ~ details
      )
    )

  out <- list(vr, dupes)

  return(out)

}
## TODO
## replace email details with category
vfile <-
  'Data/Turbovote/testing-dosomething.turbovote.org-dosomething.turbovote.org-'

out <- prepData(path=paste0(vfile,latest_file,'.csv'))

vr <- out[[1]]
dupes <- out[[2]]

powerUsers <-
  dupes %>%
  group_by(nsid) %>%
  filter(max(recordCounter)>=5) %>%
  bind_rows(vr %>% filter(nsid=='5a84b01ea0bfad5dc71768a2'))

if(dbExistsTable(pg,c("public", "turbovote_file"))) {

  q <- "truncate public.turbovote_file"
  runQuery(q,'pg')

  }
dbWriteTable(pg,c("public", "turbovote_file"), vr, append = TRUE, row.names=F)

# Analysis ----------------------------------------------------------------
library(reshape2)

##For Visuals
npPivot <- function(pivot) {

  pivot <- enquo(pivot)
  out <-
    vr %>%
    filter(!is.na(!!pivot) & (!!pivot)!='') %>%
    group_by(ds_vr_status, !!pivot) %>%
    summarise(Count=n()) %>%
    mutate(Proportion=Count/sum(Count))  %>%
    melt(value.var='Proportion') %>% as.tibble() %>%
    mutate(
      label = case_when(
        variable=='Count' ~ as.character(value),
        TRUE ~ paste0(round(value*100,1),'%')
      )
    )
  return(out)
}

uSource <- npPivot(user_source)
Source <- npPivot(source)
detSource <- npPivot(source_details)

sourceStep <-
  vr %>%
  filter(source != '') %>%
  group_by(ds_vr_status, source, details) %>%
  summarise(Count=n()) %>%
  mutate(Proportion=Count/sum(Count)) %>%
  melt(value.var='Proportion') %>% as.tibble() %>%
  mutate(
    label = case_when(
      variable=='Count' ~ as.character(value),
      TRUE ~ paste0(round(value*100,1),'%')
    )
  )

camp <-
  vr %>%
  filter(!is.na(signups)) %>%
  group_by(ds_vr_status) %>%
  summarise(
    Signups = mean(signups),
    Reportbacks = mean(reportbacks)
    ) %>%
  melt(value.var='meanRBs') %>% as.tibble()

dens <-
  vr %>%
  filter(signups < quantile(signups, .95, na.rm=T))

## For Pacing Doc

all <-
  vr %>%
  group_by(week) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

bySource <-
  vr %>%
  group_by(week, source) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    rbs = sum(reportback),
    complete_form = grepl('form', ds_vr_status) %>% sum(),
    complete_online = grepl('OVR', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  ) %>%
  melt(value.var =
         c('tot_vot_reg','rbs','complete_form',
           'complete_online','self_report')) %>%
  dcast(week ~ source + variable, value.var='value') %>%
  replace(is.na(.), 0) %>%
  select(week, starts_with('web'), starts_with('email'), starts_with('sms'),
         starts_with('social'),starts_with('part'), starts_with('no_attr'))

## For Asterisks Doc

aster <-
  vr %>%
  group_by(month, campaignId) %>%
  summarise(
    rbs = sum(reportback),
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum(),
    self_report = sum(ds_vr_status=='confirmed')
  )

## Month over month view

MoM <-
  vr %>%
  filter(created_at >= '2018-01-01') %>%
  mutate(
    date = as.Date(created_at)
  ) %>%
  group_by(date) %>%
  summarise(
    Registrations = length(which(grepl('register', ds_vr_status)))
  ) %>%
  mutate(
    month = month(date)
  ) %>%
  group_by(month) %>%
  mutate(
    registerToDate = cumsum(Registrations),
    dayOfMonth = as.numeric(format(date, "%d"))
  ) %>%
  ungroup() %>%
  select(dayOfMonth, month, registerToDate) %>%
  melt(id.var=c('dayOfMonth','month')) %>%
  mutate(month = as.factor(month))

## Excel output

library(openxlsx)

wb <- createWorkbook()

addWorksheet(wb, 'rawData')
writeData(wb, 'rawData', vr, rowNames = F)
addWorksheet(wb, 'AllSources')
writeData(wb, 'AllSources', all, rowNames=F)
addWorksheet(wb, 'bySource')
writeData(wb, 'bySource', bySource, rowNames=F)
addWorksheet(wb, 'RBAsterisk')
writeData(wb, 'RBAsterisk', aster, rowNames=F)

saveWorkbook(
  wb,
  paste0('Data/Turbovote/output_',Sys.Date(),'.xlsx'),
  overwrite = TRUE
)