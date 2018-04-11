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

collapseRace <- function(dat) {

  raceSet <- dat %>% select(Response_ID, starts_with('race'))
  raceVars <- raceSet %>% select(starts_with('race')) %>% names()

  setRace <-
    raceSet %>%
    mutate_at(
      .vars = vars(starts_with('race')),
      .funs = funs(ifelse(is.na(.),0,1))
    ) %>%
    mutate(
      ticks = rowSums(select(., contains("race."))),
      race = case_when(
        ticks > 1 ~ 'Multiracial',
        get(raceVars[1])==1 & ticks==1 ~ 'White',
        get(raceVars[2])==1 & ticks==1 ~ 'Hispanic/Latino',
        get(raceVars[3])==1 & ticks==1 ~ 'Black',
        get(raceVars[4])==1 & ticks==1 ~ 'Native American',
        get(raceVars[5])==1 & ticks==1 ~ 'Asian',
        get(raceVars[6])==1 & ticks==1 ~ 'Pacific Islander',
        TRUE ~ 'Uncertain'
      )
    ) %>%
    select(-starts_with('race.'), -ticks)
}

refactorPivots <- function(dat) {
  dat %<>%
    mutate(
      state = case_when(is.na(state) ~ 'Missing', TRUE ~ state),
      political_party =
        case_when(grepl('Other', political_party) ~ 'Other',
                  TRUE ~ political_party),
      attend_religious_services_freq =
        gsub(' because', '', attend_religious_services_freq),
      political_view =
        factor(political_view,
               levels = c('Very conservative', 'Conservative', 'Moderate',
                          'Liberal', 'Very Liberal')),
      grade_level = case_when(
        grepl('vocati', grade_level) ~ 'Associate/Technical/Vocational',
        grepl('not in school', grade_level) ~ 'Not in School',
        TRUE ~ grade_level
      ) %>%
        gsub('college/university', 'college', .) %>%
        factor(
          .,
          levels =
            c('Middle school', 'High School', 'Associate/Technical/Vocational',
              '2 year college (part-time)', '2 year college (full-time)',
              '4 year college (part-time)', '4 year college (full-time)',
              'Graduate school', 'Not in School', 'Other')
        ),
      parental_education = case_when(
        grepl('Associate', parental_education) ~ 'Associate Degree',
        grepl('Bachelor', parental_education) ~ 'Bachelors Degree',
        grepl('doctorate', parental_education) ~ 'Graduate Degree',
        grepl('High school', parental_education) ~ 'High School Diploma',
        grepl('Some college', parental_education) ~ 'Some College',
        grepl('Some high', parental_education) ~ 'Some High School',
        TRUE ~ parental_education
      ) %>%
        factor(
          .,
          levels =
            c('Some High School', 'High School Diploma', 'Some College',
              'Associate Degree', 'Bachelors Degree', 'Graduate Degree',
              "Don't know")
        ),
      attend_religious_services_freq =
        factor(
          attend_religious_services_freq,
          levels =
            c('Never, unaffiliated', 'Never, agnostic or atheist',
              'Once every few years', 'Once per year', 'Several times per year',
              'At least once per month', 'Weekly', 'Multiple times a week')
        ),
      political_party =
        factor(
          political_party,
          levels =
            c('Republican', 'Independent', 'Unaffiliated', 'Other', 'Democrat')
        )
    )
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
        age(dob) >= 13 & age(dob) <= 25 &
        Country_Code == 'US'
    )

  raceMunge <- collapseRace(combine)

  combine <-
    combine %>%
    left_join(raceMunge, by = 'Response_ID') %>%
    select(-starts_with('race.')) %>%
    rename(
      political_party = polit_party,
      state = Region
    )

  combine <-  refactorPivots(combine)

  return(combine)

}

set <- createAnalyticalSet(
  'Data/spis_members_raw_values.csv',
  'Data/spis_genpop_raw_values.csv'
  )
