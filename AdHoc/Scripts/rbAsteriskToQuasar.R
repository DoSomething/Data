source('config/init.R')
library(googlesheets)
library(zoo)

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

process.2017 <- function(sheet) {
  
  processed.2017 <- 
    sheet %>% 
    rename(
      date = X1, 
      rbs = `# Rbs not in Looker`,
      calls = `# of Phone Calls (Completed)`, 
      social = `# of Social Shares (Verified, tracked to NSID)`,
      voter_registrations = `# of Voter Registrations`,
      other = `# of Verified Additional Actions`,
      campaign_run_id = `Campaign Run ID`) %>% 
    select(date, rbs, calls, social, voter_registrations, other, campaign_run_id) %>% 
    mutate(
      dateVals = sapply(strsplit(date," "), `[`, 1) ,
      campaign = ifelse(grepl(paste(month.name, collapse="|"), date), NA, date),
      date = ifelse(grepl(paste(month.name, collapse="|"), date), date, NA),
      rbs = ifelse(is.na(rbs), 0, rbs),
      date = na.locf(date)
    ) %>% 
    filter(!is.na(campaign_run_id)) %>% 
    select(-dateVals) %>% 
    mutate_at(.vars = vars(-date,-campaign), .funs = funs(ifelse(is.na(.), 0, .))) %>%
    mutate(
      date = as.Date(paste0('1 ',date), '%d %B %Y')
    ) %>% 
    arrange(date)
  
  return(processed.2017)
  
}

process.2018 <- function(sheet) {
  
  processed.2018 <- 
    sheet %>% 
    rename(date = X1, 
           rbs = `Total # of Rbs Not in Looker (Verified, trackable to UID)`,
           calls = `# of Unique Phone Calls (Completed)`,
           voter_registrations = `# of Voter Registrations`,
           social = `# of Social Shares (Verified, tracked to NSID)`,
           social_count = `Social shares as reportbacks?`,
           other = `# of Verified Additional Actions (Multiple photos of different actions in one campaign)`,
           campaign_run_id = `Campaign Run ID`
    ) %>% 
    mutate(
      dateVals = sapply(strsplit(date," "), `[`, 1) ,
      campaign = ifelse(grepl(paste(month.name, collapse="|"), date), NA, date),
      date = ifelse(grepl(paste(month.name, collapse="|"), date), date, NA),
      date = na.locf(date),
      social = as.numeric(ifelse(social_count=='y', social, 0))
    ) %>% 
    select(date, rbs, calls, voter_registrations, social, campaign, other, campaign_run_id) %>% 
    filter(!is.na(campaign_run_id)) %>%
    mutate(
      rbs = as.numeric(gsub("[^0-9]", "",rbs)),
      date = as.Date(paste0('1 ',date), '%d %B %Y'),
      other = as.numeric(other),
      calls = as.numeric(calls)
    ) %>% 
    mutate_at(.vars = vars(-date,-campaign), .funs = funs(ifelse(is.na(.), 0, .)))
  
  return(processed.2018)
  
}
  
getDates <- function() {
  
  dates <- 
    seq.Date(as.Date('2017-01-01'),as.Date('2017-12-31'),by = 'days') %>% 
    tibble(dates = .) %>% 
    group_by(month(dates)) %>% 
    summarise(
      days = n()
    ) %>% 
    rename(
      month = `month(dates)`
    )
  
  return(dates)
}

combineOutputs <- function(sheet2017, sheet2018) {
  
  rbMon <-
    sheet2017 %>% 
    bind_rows(sheet2018) %>% 
    arrange(date)
  
  return(rbMon)
}

makeRBCSV <- function() {
  
  ####Get Sheets####
  workbookID <- getWorkbookKey('Reportbacks Ast')
  workbook <- getWorkBook(workbookID)
  sheetNames <- getSheetName(workbookID)
  sheet.2017 <- getWorksheet(sheetNames, workbook, '2017')
  sheet.2018 <- getWorksheet(sheetNames, workbook, '2018')
  
  ####Prep 2017####
  processed.2017 <- process.2017(sheet.2017)
  
  ####Prep 2018####
  processed.2018 <- process.2018(sheet.2018)
  
  ####Combine####
  rbMon <- combineOutputs(processed.2017, processed.2018)
  
  ####Return####
  return(rbMon)
  
}

reportbacks_asterisk <- makeRBCSV()

write_csv(
  reportbacks_asterisk, 
  path = '../../quasar/quasar/misc/reportbacks_asterisk.csv'
  )

system('scp ~/quasar/quasar/misc/reportbacks_asterisk.csv jump.d12g.co:/quasar-csv/reportbacks_asterisk.csv')
