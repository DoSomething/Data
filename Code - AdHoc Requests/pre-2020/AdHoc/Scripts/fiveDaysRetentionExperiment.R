source('config/init.R')
source('config/DSUtils.R')
source('config/mySQLConfig.R')
library(xlsx)
library(openxlsx)
library(scales)

extractFromWB <- function(path) {
  
  names <- getSheetNames(path)
  outset <- tbl_dt(data.table())
  
  for (i in 2:length(names)) {
    
    sheet <- openxlsx::read.xlsx(path, sheet = i) %>% tbl_dt()
    
    sheet <- tryCatch(
      sheet %>%
        select(contains('northstar', ignore.case = T)) %>% 
        select(-contains('source', ignore.case=T))
      ,
      error=function(e) e
    )
    if (any(class(sheet) == 'error')==FALSE) {
      sheet %<>% 
        mutate(
          group = names[i]
        )
      
      names(sheet) <- c('northstar_id', 'group')
      
      outset <- rbind(sheet, outset)
      
    }
    
  }
  return(tbl_dt(outset))
}

fd <- extractFromWB('Data/5Days.xlsx')

mel <- 
  read_csv('Data/september_actions.csv') %>% 
  select(-contains('X1')) %>% 
  set_names(c('northstar_id', 'event_id')) %>% 
  tbl_dt() 

activeSept <-
  fd %>% 
  mutate(
    active_sept = ifelse(northstar_id %in% mel$northstar_id, 1, 0)
  ) %>% 
  group_by(group) %>% 
  summarise(
    proportion_active_september = percent(mean(active_sept))
  )

q <- "
SELECT 
  c.northstar_id,
  c.signup_id,
  c.campaign_id,
  c.campaign_run_id,
  c.post_id
FROM quasar.campaign_activity c
WHERE c.signup_created_at >= '2017-08-01'
"

camp <- runQuery(q)

run140kControl <- 
  fd %>% 
  filter(group == 'RUN 1 40k CONTROL GROUP') %>% 
  filter(!duplicated(northstar_id)) %>% 
  mutate(
    anySignup = ifelse(northstar_id %in% camp$northstar_id, 1, 0),
    anyReportback = ifelse(northstar_id %in% (camp[post_id != -1,northstar_id]), 1, 0)
  ) %>% 
  summarise(
    proportionAnySignup = sum(anySignup),
    proportionAnyReportback = sum(anyReportback)
  )

RUN220kdbas <-
  fd %>% 
  filter(group == 'RUN 2, 20k DBAS') %>% 
  filter(!duplicated(northstar_id)) %>% 
  mutate(
    signupSucker = ifelse(northstar_id %in% camp[campaign_run_id == 7896,northstar_id], 1, 0),
    reportbackSucker = ifelse(northstar_id %in% camp[campaign_run_id == 7896 & post_id != -1,northstar_id], 1, 0)
  ) %>% 
  summarise(
    signupSucker = sum(signupSucker),
    reportbackSucker = sum(reportbackSucker)
  )

RUN220kCONTROL <-
  fd %>% 
  filter(group == 'RUN 2, 20k CONTROL') %>% 
  filter(!duplicated(northstar_id)) %>% 
  mutate(
    signupSucker = ifelse(northstar_id %in% camp[,northstar_id], 1, 0),
    reportbackSucker = ifelse(northstar_id %in% camp[post_id != -1,northstar_id], 1, 0)
  ) %>% 
  summarise(
    anySucker = sum(signupSucker),
    reportbackSucker = sum(reportbackSucker)
  )

