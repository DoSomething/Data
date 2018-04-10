source('Scripts/prepData.R')
library(rlang)
library(rpart)
library(rpart.plot)
options(useFancyQuotes = F)

buildMapping <- function(replace, inCode, outCode) {

  mapping <- exprs()

  for (i in 1:length(inCode)) {

    mapping[[i]] <-
      bquote(get(.(replace)) == .(inCode[i]) ~ .(outCode[i]))

  }

  return(mapping)

}

lookupMaker <- function(input, mapFrom, mapTo, finCode) {

  input <- enquo(input)
  input.s <- paste0(input)[2]

  recodeMapping <- buildMapping(input.s, mapFrom, mapTo)
  outcomeMapping <- buildMapping(input.s, mapFrom, finCode)

  lookupTable <-
    set %>%
    count(!!input) %>%
    mutate(
      recoded = case_when(!!!recodeMapping),
      outcome = case_when(!!!outcomeMapping)
    ) %>%
    select(-n)

  return(lookupTable)

}

rfPivotSelection <- function(tree, outcome, pivots) {

  require(caret)
  require(randomForest)

  pivots.form <-
    paste0(pivots[2]) %>%
    substr(start = 3, stop = nchar(.)-1) %>%
    gsub(', ', '+', .)
  outcome <- paste0(outcome[2])

  pivots <- unlist(strsplit(pivots.form, split="\\+"))

  tree <-
    tree %>%
    mutate(rand = runif(nrow(.), 0, 1)) %>%
    mutate_if(is.character, as.factor)

  f <- as.formula(paste0('outcome ~ ',pivots.form,'+rand'))

  rf <- randomForest(f, data=tree, importance=T)

  Imp <- tibble(
    var = row.names(varImp(rf)),
    imp = varImp(rf,scale=T)$Overall
  ) %>%
    arrange(-imp)

  cutoff <- Imp %>% filter(var=='rand') %>% select(imp) %>% as.numeric()*2
  Imp %<>%
    filter(imp > cutoff)

  keyPivots <- c()
  for (i in 1:length(pivots)) {
    if (grepl(pivots[i], paste(Imp$var, collapse="|"))) {
      keyPivots <- c(pivots[i], keyPivots)
    }
  }

  return(keyPivots)
}

analyzeStyleRank <- function(outcome, pivots, ...) {

  outcome <- enquo(outcome)
  pivots <- enquo(pivots)

  outcomeLookup <- lookupMaker(!!outcome, ...)

  forRF <-
    set %>%
    select(!!outcome, !!pivots) %>%
    left_join(outcomeLookup)

  importantPivots <- rfPivotSelection(forRF, outcome, pivots)

  return(importantPivots)
  ## Generate overall frequency overall average value
  ## Generate pivoted frequencies and avg values
  ## Plot all of the above

}

mapFrom <- c('Strongly Disagree','2','3','4','Strongly Agree')
mapTo <- c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')
finCode <- c(-2,-1,0,1,2)

pivs <-
  analyzeStyleRank(
    impact_attitudes.I_make_an_active_effort_to_understand_others_perspectives,
    pivots = c(Group, sex, fam_finances, age, race, region,
               political_party, political_view, attend_religious_services_freq),
    mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )
