source('config/init.R')

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

fixColName <- function(x) {
  # browser()
  beginNumber <- grepl('[0-9]', substr(x, 1, 1))
  secondCharNumber <- grepl('[0-9]', substr(x, 2, 2))
  if (beginNumber==T & secondCharNumber==T) {
    y <- substr(x, 6, nchar(x))
  } else if (beginNumber==T) {
    y <- substr(x, 5, nchar(x))
  } else {
    y <- x
  }
  end.s <- substr(y, nchar(y)-2, nchar(y))
  lastNumber <- grepl('[0-9]', substr(end.s, nchar(end.s), nchar(end.s)))
  secondLastNumber <- grepl('[0-9]', substr(end.s, nchar(end.s)-1, nchar(end.s)-1))
  if (lastNumber==T & secondLastNumber==T) {
    out <- substr(y, 1, nchar(y)-3)
  } else if (lastNumber==T) {
    out <- substr(y, 1, nchar(y)-2)
  } else {
    out <- y
  }
  out <- gsub(' ', '_', out)
  out <- gsub("[-/&'() ]+", '', out)
  return(out)
}

liftColFromRow1 <- function(dat) {
  # browser()
  topRow <-
    dat %>%
    filter(is.na(`Response ID`))
  for (j in 1:length(names(dat))) {
    pasteVal <- gsub('[^A-z]','_',topRow[[j]])
    pasteVal <- gsub('__', '_', pasteVal)
    fixVal <- fixColName(names(dat)[j])
    if (!is.na(pasteVal)) {
      names(dat)[j] <- paste0(fixVal,'.',pasteVal)
    } else {
      names(dat)[j] <- fixVal
    }
  }
  return(dat)
}

processSet <- function(path) {

  d <-
    suppressMessages(suppressWarnings(read_csv(path))) %>%
    select(-starts_with('Custom'))

  t <- liftColFromRow1(d)

  memberSet <-
    t %>%
    filter(!is.na(Response_ID))

  return(memberSet)
}

createAnalyticalSet <- function(memberPath, genpopPath) {

  memberSet <-
    processSet(memberPath) %>%
    mutate(Group = 'Members') %>%
    filter(
      Time_Taken_to_Complete_Seconds >=
        quantile(Time_Taken_to_Complete_Seconds, .5) / 3
    )
  genpopSet <-
    processSet(genpopPath) %>%
    mutate(Group = 'Gen Pop') %>%
    filter(
      Time_Taken_to_Complete_Seconds >=
        quantile(Time_Taken_to_Complete_Seconds, .5) / 3
      )

  combine <-
    memberSet %>%
    bind_rows(genpopSet) %>%
    mutate(
      dob = as.Date(dob, format='%m/%d/%y'),
      age = age(dob)
      ) %>%
    filter(
      Duplicate==F &
      (External_Reference!='test_response' | is.na(External_Reference)) &
      age(dob) >= 13 & age(dob) <= 25
    )

  raceVars <- set %>% select(starts_with('race')) %>% names()
  racemap <-
    list(
      raceVars[1] == 'White' & raceVars[2:7]
      )

  analyticalSet <-
    combine %>%
    mutate(
      race = case_when(!!!racemap)
    )

  return(analyticalSet)

}

set <- createAnalyticalSet(
  'Data/spis_members_raw_values.csv',
  'Data/spis_genpop_raw_values.csv'
  )
