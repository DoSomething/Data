source('Scripts/analyzeQuestionTypes.R')

mapFrom <- c('Strongly Disagree','2','3','4','Strongly Agree')
mapTo <- c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')
finCode <- c(-2,-1,0,1,2)
effortUnderstandPerspectives <-
  stylePickOneOrdinal(
    set,
    impact_attitudes.I_make_an_active_effort_to_understand_others_perspectives,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Dont know',NA,'Less than 10','10-20','More than 20')
mapTo <- c('Uncertain','Uncertain','Less than 10%','10-20%','More than 20%')
finCode <- c(0,0,1,2,3)
willingPayMoreBrandValues <-
  stylePickOneOrdinal(
    dat=set %>% filter(!is.na(willing_to_pay_how_much_more_brand_good_values)),
    willing_to_pay_how_much_more_brand_good_values,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Not at all important','2','3','4','Very Important')
mapTo <- c('Not Important','A Little','Neutral','Important','Very Important')
finCode <- c(-2,-1,0,1,2)
howImportantVoterReg <-
  stylePickOneOrdinal(
    set,
    causes_importance.Voter_registration,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Not at all influential','2','3','4','Very influential')
mapTo <- c('Not Influential','A Little','Neutral','Influential','Very Influential')
finCode <- c(-2,-1,0,1,2)
howInfluentialQuality <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Quality_of_product_service,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('No','Yes')
mapTo <- c('No','Yes')
finCode <- c(0,1)
volunteerInformal <-
  stylePickOneOrdinal(
    set,
    volunteer_informal,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Not very likely','Not likely','Neutral',
             'Dont know it would depend on the issue','Likely','Very likely','Very Likely')
mapTo <- c('Not Very Likely','Not Likely','Neutral','Neutral','Likely',
           'Very Likely','Very Likely')
finCode <- c(-2,-1,0,0,1,2,2)
howLikelyStudentMarch <-
  stylePickOneOrdinal(
    set,
    how_likely_student_march,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Not at all influential','2','3','4','Very influential')
mapTo <- c('Not Influential','A Little','Neutral','Influential','Very Influential')
finCode <- c(-2,-1,0,1,2)
howInfluentialQuality <-
  stylePickOneOrdinal(
    set,
    influence_when_purchasing.Quality_of_product_service,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

mapFrom <- c('Never','One time only','Once every few months',
             'About once a month','2-3 times a month','About once a week',
             'More than once a week')
mapTo <- c('Never','One time only','Once every few months',
           'About once a month','2-3 times a month','About once a week',
           'More than once a week')
finCode <- c(1,2,3,4,5,6,7)
volunteerFrequency <-
  stylePickOneOrdinal(
    set,
    volunteer_freq,
    pivots = c(Group, sex, fam_finances, age, race, region, parental_education,
               political_party, political_view, attend_religious_services_freq,
               grade_level),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )

issuesTakenAction <-
  styleSelectMultiple(
    set,
    'which_issues_taken_action_12mo.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level)
  )

productsUsed <-
  styleSelectMultiple(
    set,
    'products_used_past12mo.',
    pivots=c(Group, sex, fam_finances, age, race, region, parental_education,
             political_party, political_view, attend_religious_services_freq,
             grade_level)
  )
