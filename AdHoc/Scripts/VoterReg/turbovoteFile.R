source('config/init.R')
source('config/mySQLConfig.R')

# Data prep ---------------------------------------------------------------

getData <- function(path) {
  vr <- 
    read_csv(path) %>% 
    filter(
      !grepl('thing.org', email) & 
        !grepl('testing', hostname) & 
        !is.na(`referral-code`)
    ) 
  
  for (i in 1:length(names(vr))) {
    if(grepl('-', names(vr)[i])) {
      names(vr)[i] <- gsub('-','_',names(vr)[i])
    }
  }
  return(vr)
}

vr <- getData('Data/testing-dosomething.turbovote.org-2018-03-06.csv')

processReferralColumn <- function(dat) {
  maxSep <- max(as.numeric(names(table(str_count(dat$referral_code, ',')))))+1
  parsedSep <- 
    dat %>% 
    select(id, referral_code) %>% 
    separate(referral_code, LETTERS[1:maxSep], ',', remove = F) %>% 
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
      campaignRunId = ifelse(is.na(campaignId), '8022',
                             ifelse(campaignId=='8017','8022',campaignRunId)),
      campaignId = ifelse(is.na(campaignRunId), '8017',
                          ifelse(campaignRunId=='8022','8017',campaignId)),
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
        source_details == '11_facts' ~ 'web',
        TRUE ~ source
      )
    ) %>% 
    select(-A,-B,-C,-D,-E)
  return(parsedSep)
}

refParsed <- processReferralColumn(vr)
