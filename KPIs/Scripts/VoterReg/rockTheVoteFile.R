
prepRTVFile <- function(path) {

  data <-
    suppressWarnings(suppressMessages(read_csv(path))) %>%
    filter(
      !grepl('dosomething', `Email address`) &
      !grepl('testing', `Tracking Source`) &
      !grepl('lkpttn@gmail.com', `Email address`)
    )

  for (i in 1:length(names(data))) {
    if(grepl('-', names(data)[i])) {
      names(data)[i] <- gsub('-','_',names(data)[i])
    } else if(grepl(' ', names(data)[i])) {
      names(data)[i] <- gsub(' ','_',names(data)[i])
    }
  }

  data %<>%
    mutate(
      id=sapply(
        paste(
          Email_address,
          Started_registration,
          Status,
          Tracking_Source),
        digest,algo='md5'
        )
    ) %>%
    filter(!duplicated(id))

  return(data)

}

processTrackingSource <- function(dat) {

  maxSep <- max(as.numeric(names(table(str_count(rtv$Tracking_Source, ',')))))+1

  parsedSep <-
    rtv %>%
    select(id, Tracking_Source) %>%
    mutate(
      Tracking_Source = gsub('iframe\\?r=','',Tracking_Source),
      Tracking_Source = gsub('iframe','',Tracking_Source)
    ) %>%
    separate(Tracking_Source, LETTERS[1:maxSep], ',',remove = F) %>%
    mutate(
      nsid =
        case_when(
          substr(A, 1, 4)=='user' ~ substr(A, 6, nchar(A)),
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
          TRUE ~ ''
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
        source == 'web' &
          grepl('redirect', source_details) ~ 'redirect',
        source == 'web' &
          grepl('hellobar', source_details) ~ 'hellobar',
        source == 'web' &
          grepl('affirmation', source_details) ~ 'affirmation'
      )
    )

}

rtv <- prepRTVFile('Data/RockTheVote/rock_the_vote_05222018.csv')
