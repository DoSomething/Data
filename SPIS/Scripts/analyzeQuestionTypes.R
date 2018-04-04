source('Scripts/prepData.R')
options(useFancyQuotes = F)

likertMapping <- list(
  quo(get(input.s) == 'Strongly Disagree' ~ 'Strongly Disagree'),
  quo(get(input.s) == '2' ~ 'Disagree'),
  quo(get(input.s) == '3' ~ 'Neutral'),
  quo(get(input.s) == '4' ~ 'Agree'),
  quo(get(input.s) == 'Strongly Agree' ~ 'Strongly Agree')
)

likertLookupMaker <- function(input, mapping) {

  input <- enquo(input)
  input.s <- paste0(input)[2]
  var_name <- quo_name(input)

  lookupTable <-
    set %>%
    mutate(!!var_name := case_when(!!!mapping)) %>%
    count(!!input) %>%
    mutate(
      outcome =
        case_when(
          !!input == 'Strongly Disagree' ~ -2,
          !!input == 'Disagree' ~ -1,
          !!input == 'Neutral' ~ 0,
          !!input == 'Agree' ~ 1,
          !!input == 'Strongly Agree' ~ 2,
          TRUE ~ NA_real_
        )
    ) %>%
    select(-n)

  return(lookupTable)

}

likertLookupMaker(impact_attitudes.I_make_an_active_effort_to_understand_others_perspectives, mapping)
