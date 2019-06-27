source('config/init.R')
library(rlang)
library(glue)
library(gridExtra)
source('config/pgConnect.R')
pg <- pgConnect()

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {

  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))

  return(calc.age)

}

fixColName <- function(x) {

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
    suppressMessages(suppressWarnings(read_csv(path, guess_max=3000))) %>%
    select(-starts_with('Custom'))

  t <- liftColFromRow1(d)

  memberSet <-
    t %>%
    filter(!is.na(Response_ID))

  return(memberSet)

}

recodeCheckAllApply <- function(dat) {

  meetCriteria <- function(var) {

    cnt <- dat %>% count(!!sym(var))

    vals <- cnt %>% nrow()
    incNA <-  cnt %>% pull(!!sym(var)) %>% unique() %>% anyNA()

    if (vals <=2 && incNA==T) {
      return(TRUE)
    } else {
      return(FALSE)
    }

  }

  recodeThese <- c()

  for (j in 1:length(names(dat))) {

    thisVar <- names(dat)[j]

    if (meetCriteria(thisVar)==T) {

      recodeThese <- c(thisVar, recodeThese)

    }

  }

  dat %<>%
    mutate_at(
      .vars = vars(recodeThese),
      .funs = funs(ifelse(is.na(.), 0, 1))
      )

  return(dat)

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
        get(raceVars[7])==1 & ticks==1 ~ 'Arab',
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
              'Not in School', 'Other','4 year college (part-time)',
               '4 year college (full-time)','Graduate school')
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
              "Don't know",'Associate Degree', 'Bachelors Degree',
              'Graduate Degree')
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
            c('Republican', 'Libertarian', 'Independent', 'Unaffiliated', 'Other', 'Democrat')
        ),

      fam_finances =
        factor(
          fam_finances,
          levels =
            c('Struggling','Below Average','Average',"I Don't Know",'Comfortable','Very Comfortable')
        )

    )

  return(dat)

}

recodeBinaryToCharacter <- function(dat) {

  dat %<>%
    mutate_at(
      .vars = vars(starts_with('since_engage_DS.')),
      .funs = funs(ifelse(.==1,'Yes','No'))
    )

}

addSurveyWeights <- function(dat) {
  browser()
  popEst <-
    list(
      race = tibble(white = .6, non_white = .4),
      sex = tibble(other = .01, male = .505, female = .485),
      age = tibble(all = 1/13),
      education = tibble(some_highschool = .116, high_school = .295,
                         some_college = .166, associate = .098,
                         bach_degree = .205, grad_degree=.12)
    )

  calcWeights <- function(subDat, popEst) {

    raceWeights <-
      subDat %>%
      mutate(

        race_cat =
          case_when(
            race == 'White' ~ 'white',
            TRUE ~ 'non_white'
          )

      ) %>%
      count(race_cat) %>%
      mutate(
        pct = n/sum(n),

        raceWeight =
          case_when(
            race_cat == 'white' ~ popEst$race$white / pct,
            race_cat != 'white' ~ popEst$race$non_white / pct,
            TRUE ~ 1
          )
      )

    genderWeights <-
      subDat %>%
      mutate(
        gender_cat =
          case_when(
            sex == 'Man' ~ 'male',
            sex == 'Woman' ~ 'female',
            TRUE ~ 'other'
          )
      ) %>%
      count(gender_cat) %>%
      mutate(
        pct = n/sum(n),

        genderWeight =
          case_when(
            gender_cat == 'male' ~ popEst$sex$male / pct,
            gender_cat == 'female' ~ popEst$sex$female / pct,
            gender_cat == 'other' ~ popEst$sex$other / pct,
            TRUE ~ 1
          )
      )

    ageWeights <-
      subDat %>%
      count(age) %>%
      mutate(
        pct = n/sum(n),
        ageWeight = popEst$age$all / pct
      )

    eduWeights <-
      subDat %>%
      mutate(
        edu_cat =
          case_when(
            parental_education == 'Some High School' ~ 'some_highschool',
            parental_education == 'High School Diploma' ~ 'high_school',
            parental_education == 'Some College' ~ 'some_college',
            parental_education == 'Associate Degree' ~ 'associate',
            parental_education == 'Bachelors Degree' ~ 'bach_degree',
            parental_education == 'Graduate Degree' ~ 'grad_degree',
            TRUE ~ 'other'
          )
      ) %>%
      count(edu_cat) %>%
      mutate(
        pct = n/sum(n),

        eduWeight =
          case_when(
            edu_cat == 'some_highschool' ~ popEst$education$some_highschool / pct,
            edu_cat == 'high_school' ~ popEst$education$high_school / pct,
            edu_cat == 'some_college' ~ popEst$education$some_college / pct,
            edu_cat == 'associate' ~ popEst$education$associate / pct,
            edu_cat == 'bach_degree' ~ popEst$education$bach_degree / pct,
            edu_cat == 'grad_degree' ~ popEst$education$grad_degree / pct,
            TRUE ~ 1
          )
      )

    idWeights <-
      subDat %>%
      select(Response_ID, age, race, sex, parental_education) %>%
      left_join(ageWeights, by = 'age') %>%
      mutate(

        raceWeight =
          case_when(
            race == 'White' ~ filter(raceWeights, race_cat=='white') %$% raceWeight,
            race != 'White' ~ filter(raceWeights, race_cat=='non_white') %$% raceWeight
          ),

        genderWeight =
          case_when(
            sex == 'Man' ~ filter(genderWeights, gender_cat=='male') %$% genderWeight,
            sex == 'Woman' ~ filter(genderWeights, gender_cat=='female') %$% genderWeight,
            !sex %in% c('Man','Woman') ~ filter(genderWeights, gender_cat=='other') %$% genderWeight
          ),

        eduWeight =
          case_when(
            parental_education == 'Some High School' ~
              filter(eduWeights, edu_cat=='some_highschool') %$% eduWeight,
            parental_education == 'High School Diploma' ~
              filter(eduWeights, edu_cat=='high_school') %$% eduWeight,
            parental_education == 'Some College' ~
              filter(eduWeights, edu_cat=='some_college') %$% eduWeight,
            parental_education == 'Associate Degree' ~
              filter(eduWeights, edu_cat=='associate') %$% eduWeight,
            parental_education == 'Bachelors Degree' ~
              filter(eduWeights, edu_cat=='bach_degree') %$% eduWeight,
            parental_education == 'Graduate Degree' ~
              filter(eduWeights, edu_cat=='grad_degree') %$% eduWeight,
            TRUE ~ 1
          )
      ) %>%
      mutate(
        weight =  ageWeight * genderWeight  * raceWeight
      ) %>%
      select(Response_ID, eduWeight, ageWeight, genderWeight, raceWeight, weight)

    return(idWeights)

  }

  gp <- dat %>% filter(Group == 'Gen Pop') %>% calcWeights(., popEst)
  mem <- dat %>% filter(Group == 'Members') %>% calcWeights(., popEst)

  both <- bind_rows(gp, mem)

  out <-
    dat %>%
    left_join(both, by = 'Response_ID')

  return(out)

}

removeSpecialCharacters <- function(x) {
  x <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", x)
}

addCampaignActivity <- function(data, northstars) {

  q <-
    glue_sql(
      "SELECT
        s.northstar_id as \"External_Reference\",
        CASE
          WHEN u.source = 'sms' THEN 'sms'
          ELSE 'web' END AS user_channel,
        count(DISTINCT s.id) AS signups,
        sum(r.reportback_volume) AS reportbacks
      FROM signups s
      LEFT JOIN users u ON s.northstar_id = u.northstar_id
      LEFT JOIN reportbacks r ON s.id = r.signup_id
      WHERE s.northstar_id IN ({nsids*})
      GROUP BY s.northstar_id, u.source",
      .con = pg,
      nsids = northstars
    )

  qres <- runQuery(q)

  data <-
    data %>%
    left_join(qres, by='External_Reference') %>%
    mutate(
      signups =
        factor(
          case_when(
            Group == 'Gen Pop' ~ 'Gen Pop',
            is.na(signups) & Group == 'Members' ~ 'No Signups',
            signups == 1 ~ '1',
            signups > 1 & signups < 4 ~ '1-3',
            signups >= 4 & signups < 9 ~ '4-8',
            TRUE ~ '8+'
          ),
        levels=c('Gen Pop','No Signups','1','1-3','4-8','8+')
        ),
      reportbacks =
        factor(
          case_when(
            Group == 'Gen Pop' ~ 'Gen Pop',
            is.na(reportbacks) & Group == 'Members' ~ 'No Reportbacks',
            reportbacks == 1 ~ '1',
            reportbacks > 1 & reportbacks < 4 ~ '1-3',
            reportbacks >= 4 & reportbacks < 8 ~ '4-7',
            TRUE ~ '8+'
          ),
          levels=c('Gen Pop','No Reportbacks','1','1-3','4-7','8+')
        )
    )

  return(data)

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
      dob = as.Date(dob, format='%m/%d/%Y'),
      age = ifelse(is.na(dob), Age, age(dob))
    ) %>%
    filter(
      Duplicate==F &
        (External_Reference!='test_response' | is.na(External_Reference)) &
        age >= 13 & age <= 25 &
        Country_Code == 'US' &
        IP_Address != '104.139.91.20'
    ) %>%
    select(-Age)

  raceMunge <- collapseRace(combine)

  combine <-
    combine %>%
    left_join(raceMunge, by = 'Response_ID') %>%
    select(-starts_with('race.')) %>%
    rename(
      political_party = polit_party,
      state = Region
    )

  combine <- refactorPivots(combine)
  combine <- recodeCheckAllApply(combine)
  combine <- recodeBinaryToCharacter(combine)
  combine <- addSurveyWeights(combine)
  combine <- addCampaignActivity(
    combine,
    combine %>% filter(!is.na(External_Reference)) %$% External_Reference
  )
  combine <-
    combine %>%
    mutate_if(
     is.character, removeSpecialCharacters
    ) %>%
    mutate(
      which_issues_taken_action_12mo.Sexual_harassment_assault =
        pmax(which_issues_taken_action_12mo.Sexual_harassment_assault,
             which_issues_taken_action_12mo.Sexual_harassment_and_assault)
    ) %>%
    select(-which_issues_taken_action_12mo.Sexual_harassment_and_assault)

  return(combine)

}

set <-
  createAnalyticalSet(
    'Data/2019 Q1/spis_members_raw_values.csv',
    'Data/2019 Q1/spis_genpop_raw_values.csv'
  )

dbWriteTable(pg, c("survey","\"2019_spis\""), set, row.names=F)
runQuery("GRANT SELECT ON survey.\"2019_spis\" TO dsanalyst,looker")