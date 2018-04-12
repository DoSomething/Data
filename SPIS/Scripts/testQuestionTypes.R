source('Scripts/analyzeQuestionTypes.R')

analysis <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_make_an_active_effort_to_understand_others_perspectives,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )
mapFrom <- c('Strongly Disagree','2','3','4','Strongly Agree')
mapTo <- c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')
finCode <- c(-2,-1,0,1,2)

mapFrom <- c("Don't know",NA,'Less than 10%','10-20%','More than 20%')
mapTo <- c('Uncertain','Uncertain','Less than 10%','10-20%','More than 20%')
finCode <- c(0,0,1,2,3)
analysis <-
  stylePickOneOrdinal(
    dat=set %>% filter(!is.na(willing_to_pay_how_much_more_brand_good_values)),
    willing_to_pay_how_much_more_brand_good_values,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c("Don't know",'Less than 10%','10-20%','More than 20%')
mapTo <- c('Uncertain','Uncertain','Less than 10%','10-20%','More than 20%')
finCode <- c(0,1,2,3)
analysis <-
  stylePickOneOrdinal(
    dat=set %>% filter(!is.na(willing_to_pay_how_much_more_brand_good_values)),
    willing_to_pay_how_much_more_brand_good_values,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Yes','No')
mapTo <- c('Yes','No')
finCode <- c(1,0)
analysis <-
  stylePickOneOrdinal(
    set,
    volunteer_informal,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )
