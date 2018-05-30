
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
    separate(Tracking_Source, LETTERS[1:maxSep], ',',remove = F)

}

rtv <- prepRTVFile('Data/RockTheVote/rock_the_vote_05222018.csv')
