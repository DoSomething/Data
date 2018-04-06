source('Scripts/prepData.R')
library(rlang)
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

analyzeStyleRank <- function(outcome, pivots, ...) {
  browser()

  outcome <- enquo(outcome)
  pivots <- enquo(pivots)

  outcomeLookup <- lookupMaker(!!outcome, ...)

  forTree <-
    set %>%
    select(!!outcome, !!pivots) %>%
    left_join(outcomeLookup)

  ## Insert decision tree to select pivots
  ## Generate overall frequency overall average value
  ## Generate pivoted frequencies and avg values
  ## Plot all of the above

}

mapFrom <- c('Strongly Disagree','2','3','4','Strongly Agree')
mapTo <- c('Strongly Disagree','Disagree','Neutral','Agree','Strongly Agree')
finCode <- c(-2,-1,0,1,2)

analyzeStyleRank(
  impact_attitudes.I_make_an_active_effort_to_understand_others_perspectives,
  pivots = c(sex, fam_finances),
  mapFrom = mapFrom, mapTo = mapTo, finCode=finCode
  )
