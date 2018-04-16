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
  browser()
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
    geom_bar(stat='count', width = .65, fill='skyblue2') +
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
        p <- p + geom_bar(aes(x=get(pivots[i]), y=avgVal),
                          fill='skyblue2', stat='identity')
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

  freqPlot <- getFrequencyPlot(thisQuestionSet, recoded, mapTo, outcome)

  pivPlot <- getPivotPlots(thisQuestionSet, importantPivots)
  groupPivPlot <- getPivotPlots(thisQuestionSet, importantPivots, 'Group')

  analysis <- list(freqPlot, pivPlot, groupPivPlot)

  return(analysis)

}



styleSelectMultiple <- function(dat, questionSuffix, pivots) {
  require(reshape2)
  require(corrplot)

  pivots <- enquo(pivots)
  thisQuestionSet <-
    dat %>%
    select(!!pivots) %>%
    bind_cols(dat %>% select(starts_with(questionSuffix))) %>%
    select(-ends_with('Other'), -ends_with('None_of_these')) %>%
    mutate(
      outcome = rowSums(select(., starts_with(questionSuffix)))
    )

  forCorr <- thisQuestionSet %>% select(starts_with(questionSuffix))
  names(forCorr) <- gsub(questionSuffix, '', names(forCorr))

  forCorr <- forCorr %>% cor(.)
  corPlot <- corrplot(forCorr, method="pie")

  corDat <-
    forCorr %>%
    melt() %>%
    filter(value < 1) %>%
    group_by(Var1) %>%
    filter(value == max(value)) %>%
    setNames(c('variable','top_cor','cor'))

  ovr <-
    thisQuestionSet %>%
    select(starts_with(questionSuffix)) %>%
    summarise_all(mean) %>%
    melt() %>%
    mutate(
      variable = str_replace_all(variable, questionSuffix, '')
      ) %>%
    left_join(corDat) %>%
    mutate(pos = pmax(value/2, .2))

  ovr.p <-
    ggplot(ovr, aes(x=reorder(variable, -value), y=value, fill=cor)) +
    geom_bar(stat='identity') +
    geom_text(
      aes(y=pos,label=paste('Top Corr: ',top_cor, ' = ', round(cor, 2))),
      size=3, angle=90
      ) +
    labs(x=paste0('Average # Ticked = ', round(sum(ovr$value), 3)),
         title=questionSuffix,y='Percentage Ticked') +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.title = element_text(hjust = .5)
      ) +
    scale_fill_gradientn(colours=rev(terrain.colors(2)))

  # keyPivots <- rfPivotSelection(thisQuestionSet, quo(outcome), pivots)

  out <- list(corPlot, ovr.p)

  return(out)

}

styleSelectMultiple(
  set,
  'products_used_past12mo.',
  pivots=c(Group, sex, fam_finances, age, race)
)
