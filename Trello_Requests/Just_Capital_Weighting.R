library(tidyverse)
library(data.table)
library(plyr)

#create race function
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
      ticks = rowSums(select(., contains("race_"))),
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


#####################################################################################################
drivers1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 1 Factors.xls")
drivers2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 2 Factors.xls")
drivers <- rbind(factors1,factors2)
#identify race
drivers_race <- collapseRace(drivers) %>%
  select(Response_ID, race)
#join race variable to dataset
drivers <-merge(x=drivers, y=drivers_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
drivers <- drivers %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
environment1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 1 Environment.xls")
environment2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 2 Environment.xls")
environment <- rbind(environment1,environment2)

#identify race
environment_race <- collapseRace(environment) %>%
  select(Response_ID, race)
#join race variable to dataset
environment<-merge(x=environment, y=environment_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
environment <- environment %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
jobs1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 1 Jobs.xls")
jobs2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 2 Jobs.xls")
jobs <- rbind(jobs1,jobs2)

#identify race
jobs_race <- collapseRace(jobs) %>%
  select(Response_ID, race)
#join race variable to dataset
jobs <-merge(x=jobs, y=jobs_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
jobs <- jobs %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
customers1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 1 Customers.xls")
customers2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 2 Customers.xls")
customers <- rbind(customers1,customers2)

#identify race
customers_race <- collapseRace(customers) %>%
  select(Response_ID, race)
#join race variable to dataset
customers <-merge(x=customers, y=customers_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
customers <- customers %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
managers1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 3 Managers.xls")
managers2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 4 Managers .xls")
managers <- rbind(managers1,managers2)

#identify race
managers_race <- collapseRace(managers) %>%
  select(Response_ID, race)
#join race variable to dataset
managers <-merge(x=managers, y=managers_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
managers<- managers %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
product1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 3 Product.xls")
product2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 4 Product.xls")
product <- rbind(product1,product2)

#identify race
product_race <- collapseRace(product) %>%
  select(Response_ID, race)
#join race variable to dataset
product <-merge(x=product, y=product_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
product <- product %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
communities1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 4 Data Communities.xls")
communities2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 3 Data Communities.xls")
communities <- rbind(communities1,communities2)

#identify race
communities_race <- collapseRace(communities) %>%
  select(Response_ID, race)
#join race variable to dataset
communties <-merge(x=communities, y=communities_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
communities <- communities %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
workers1 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 4 Data Workers.xls")
workers2 <- read_excel("~/Documents/Just Capital /Data/Weighting/JUST CAPITAL Survey 3 Data Workers.xls")
workers <- rbind(workers1,workers2)

#identify race
workers_race <- collapseRace(workers) %>%
  select(Response_ID, race)
#join race variable to dataset
workers <-merge(x=workers, y=workers_race, by ="Response_ID", all=TRUE)
#filter out <13 yr and 25+ yr
workers <- workers %>%
  filter(age!= "Younger than 13" & age!= "Older than 25")

#####################################################################################################
drivers <- drivers%>%
  mutate(sex = as.numeric(sex))


#weights
addSurveyWeights <- function(dat) {
  
  popEst <-
    list(
      race = tibble(white = .6, hispanic = .18, black = .16, native_american = .01, asian = .04, multiracial = .02, pacific = .0002),
      sex = tibble(other = .01, male = .505, female = .485),
      age = tibble(all = 1/13)
    ) 
  
  calcWeights <- function(subDat, popEst) {
    
    raceWeights <-
      subDat %>%
      count(race) %>%
      mutate(
      pct = n/sum(n),
        
        raceWeight =
          case_when(
            race == 'White' ~ popEst$race$white / pct,
            race == 'Hispanic/Latino' ~ popEst$race$hispanic / pct,
            race == 'Black' ~ popEst$race$black / pct,
            race == 'Native American' ~ popEst$race$native_american / pct,
            race == 'Asian' ~ popEst$race$asian / pct,
            race == 'Pacific Islander' ~ popEst$race$pacific / pct,
            race == 'Multiracial' ~ popEst$race$multiracial / pct,
            TRUE ~ 1
          )
      )
    
    genderWeights <-
      subDat %>%
      mutate(
        gender_cat =
          case_when(
            sex == 'Male' ~ 'male',
            sex == 'Female' ~ 'female',
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
    
    
    idWeights <-
      subDat %>%
      select(Response_ID, age, race, sex) %>%
      left_join(ageWeights, by = 'age') %>%
      mutate(
        
        raceWeight =
          case_when(
            race == 'White' ~ filter(raceWeights, race=='White') %$% raceWeight,
            race == 'Hispanic/Latino' ~ filter(raceWeights, race=='Hispanic/Latino') %$% raceWeight,
            race == 'Black' ~ filter(raceWeights, race=='Black') %$% raceWeight,
            race == 'Asian' ~ filter(raceWeights, race=='Asian') %$% raceWeight,
            race == 'Pacific Islander' ~ filter(raceWeights, race=='Pacific Islander') %$% raceWeight,
            race == 'Multiracial' ~ filter(raceWeights, race=='Multiracial') %$% raceWeight,
            race == 'Uncertain' ~ filter(raceWeights, race=='Uncertain') %$% raceWeight,
            race == 'Native American' ~ filter(raceWeights, race=='Native American') %$% raceWeight
          ),
        
        genderWeight =
          case_when(
            sex == 'Male' ~ filter(genderWeights, gender_cat=='male') %$% genderWeight,
            sex == 'Female' ~ filter(genderWeights, gender_cat=='female') %$% genderWeight,
            !sex %in% c('Male','Female') ~ filter(genderWeights, gender_cat=='other') %$% genderWeight
          )
      ) %>%
      mutate(
        weight =  ageWeight * genderWeight  * raceWeight
      )  %>% select(Response_ID, ageWeight, genderWeight, raceWeight, weight)
    
    return(idWeights)
    
  }
  out <- calcWeights(drivers, popEst)
  
}

###################################################################################################
drivers_weights <- addSurveyWeights(drivers)
#join race variable to dataset
drivers_final <-merge(x=drivers, y=drivers_weights, by ="Response_ID", all=TRUE)
#Check weights
drivers_final %>% group_by(sex) %>% summarise(sum(weight))
table(drivers$sex)
drivers_final %>% group_by(race) %>% summarise(sum(weight))
table(drivers$race)
drivers_final %>% group_by(age) %>% summarise(sum(weight))
table(drivers$age)
