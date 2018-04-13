buildMapping <- function(replace, inCode, outCode) {

  mapping <- exprs()

  for (i in 1:length(inCode)) {

    if (!is.na(inCode[i])) {

      mapping[[i]] <-
        bquote(get(.(replace)) == .(inCode[i]) ~ .(outCode[i]))

    } else {

      mapping[[i]] <-
        bquote(is.na(get(.(replace))) ~ .(outCode[i]))

    }

  }

  return(mapping)

}

lookupMaker <- function(dat, input, mapFrom, mapTo, finCode) {

  input <- enquo(input)
  input.s <- paste0(input)[2]

  recodeMapping <- buildMapping(input.s, mapFrom, mapTo)
  outcomeMapping <- buildMapping(input.s, mapFrom, finCode)

  lookupTable <-
    dat %>%
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
    mutate(
      rand1 = runif(nrow(.), 0, 1),
      rand2 = runif(nrow(.), 0, 1)
      ) %>%
    mutate_if(is.character, as.factor)

  if (length(unique(tree$outcome)) < 3) {
    tree$outcome <- as.factor(tree$outcome)
  }

  f <- as.formula(paste0('outcome ~ ',pivots.form,'+rand1+rand2'))

  rf <- randomForest(f, data=tree, importance=T)

  rfImportance <- varImp(rf,scale=T)
  rfImportance <- setNames(rfImportance, 'Overall')

  Imp <- tibble(
    var = row.names(rfImportance),
    imp = rfImportance$Overall
  ) %>%
    arrange(-imp)

  cut1 <- Imp %>% filter(var=='rand1') %$% imp*2
  cut2 <- Imp %>% filter(var=='rand2') %$% imp*2

  Imp %<>%
    filter(
      imp > cut1 & imp > cut2 & imp > 4
      )

  keyPivots <- c()
  for (i in 1:length(pivots)) {

    if (grepl(pivots[i], paste(Imp$var, collapse="|"))) {

      keyPivots <- c(pivots[i], keyPivots)

    }

  }

  return(keyPivots)

}

getFrequencyPlot <- function(dat, toPlot, levels, title) {

  toPlot <- enquo(toPlot)
  levels <- unique(levels)

  dat %<>%
    mutate(
      plotVar = factor(!!toPlot, levels = levels)
    )
  labx <- mean(dat$outcome)
  laby <- dat %>% count(outcome) %$% max(n)*.8
  p <-
    ggplot(dat, aes(x = outcome)) +
    geom_bar(stat='count', width = .65) +
    geom_vline(xintercept = mean(dat$outcome)) +
    labs(title = paste(title[2])) +
    scale_x_continuous(labels=levels, breaks=sort(unique(dat$outcome))) +
    annotate("text", x=labx, y=laby, size=3, angle = 90, vjust=-1,
             label = paste("Average = ", round(labx, 2)))

  return(p)
}

getPivotPlots <- function(dat, pivots, specialPivot=NULL) {
  require(gridExtra)

  myplots <- list()

  for (i in 1:length(pivots)) {
    if (!is.null(specialPivot)) {
      thesePivots <- c(pivots[i],specialPivot)
    } else {
      thesePivots <- pivots[i]
      }
    avgVals <-
      dat %>%
      group_by_at(vars(one_of(thesePivots))) %>%
      summarise(
        avgVal = mean(outcome)
      )

    local({
      i <- i
      p <-
        ggplot(avgVals) +
        labs(x='', y='', title=pivots[i]) +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position="none"
        )
      if (!is.null(specialPivot)) {
        p <- p +
          geom_bar(
            aes(x=get(pivots[i]), y=avgVal, fill=get(specialPivot)),
            stat='identity', position='dodge')
      } else {
        p <- p + geom_bar(aes(x=get(pivots[i]), y=avgVal), stat='identity')
      }
      myplots[[i]] <<- p
    })
  }

  outPlot <- arrangeGrob(grobs=myplots, ncol=3)

  return(outPlot)

}

stylePickOneOrdinal <- function(dat, outcome, pivots, ...) {

  outcome <- enquo(outcome)
  pivots <- enquo(pivots)

  outcomeLookup <- lookupMaker(dat, !!outcome, ...)

  thisQuestionSet <-
    dat %>%
    select(!!outcome, !!pivots) %>%
    left_join(outcomeLookup)

  importantPivots <- rfPivotSelection(thisQuestionSet, outcome, pivots)
  # importantPivots <-
  #   c("attend_religious_services_freq", "political_view", "political_party",
  #     "race", "age", "sex", "Group", "grade_level" )

  freqPlot <- getFrequencyPlot(thisQuestionSet, recoded, mapTo, outcome)

  pivPlot <- getPivotPlots(thisQuestionSet, importantPivots)
  groupPivPlot <- getPivotPlots(thisQuestionSet, importantPivots, 'Group')

  analysis <- list(freqPlot, pivPlot, groupPivPlot)

  return(analysis)

}

styleSelectMultiple <- function(dat, questionSuffix, pivots) {
  browser()



}