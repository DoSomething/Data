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

  require(randomForest)
  require(caret)
  browser()
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
      rand2 = runif(nrow(.), 0, 1),
      rand3 = runif(nrow(.), 0, 1),
      rand4 = runif(nrow(.), 0, 1),
      rand5 = runif(nrow(.), 0, 1)
      ) %>%
    mutate_if(is.character, as.factor)

  if (length(unique(tree$outcome)) < 3) {
    tree$outcome <- as.factor(tree$outcome)
  }

  f <- as.formula(paste0('outcome ~ ',pivots.form,'+rand1+rand2+rand3+rand4+rand5'))

  rf <- randomForest(f, data=tree, importance=T, ntree=500)

  rfImportance <- varImp(rf,scale=T)
  rfImportance <- setNames(rfImportance, 'Overall')

  Imp <- tibble(
    var = row.names(rfImportance),
    imp = rfImportance$Overall
  ) %>%
    arrange(-imp)

  cut1 <- Imp %>% filter(var=='rand1') %$% imp*2
  cut2 <- Imp %>% filter(var=='rand2') %$% imp*2
  cut3 <- Imp %>% filter(var=='rand3') %$% imp*2
  cut4 <- Imp %>% filter(var=='rand4') %$% imp*2
  cut5 <- Imp %>% filter(var=='rand5') %$% imp*2

  Imp %<>%
    filter(
      imp > cut1 & imp > cut2 &
      imp > cut3 & imp > cut4 &
      imp > cut5 & imp > 4
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

  plotDat <-
    dat %>%
    group_by(outcome) %>%
    summarise(
      count = sum(weight)
    )

  labx <- weighted.mean(dat$outcome, dat$weight)
  laby <- dat %>% count(outcome) %$% max(n)*.8

  p <-
    ggplot(plotDat, aes(x=outcome, y=count)) +
    geom_bar(stat='identity', width = .65, fill='skyblue2') +
    geom_vline(xintercept = labx) +
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
        avgVal = weighted.mean(outcome, weight)
      )

    local({
      i <- i

      p <-
        ggplot(avgVals) +
        labs(x='', y='', title=pivots[i]) +
        scale_fill_brewer(palette='Set2') +
        scale_colour_brewer(palette='Set2') +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position="none"
        )

      if (!is.null(specialPivot)) {

        p <- p +
          geom_bar(
            aes(x=get(pivots[i]), y=avgVal, fill=get(specialPivot)),
            stat='identity', position='dodge', alpha=.8)

        if (class(dat[[pivots[i]]])!='character') {

          p <- p +
            geom_smooth(
              aes(x=as.numeric(get(pivots[i])), y=avgVal, color=get(specialPivot)),
              method='lm', se=F, linetype='dashed', size=.75
              )

        }

      } else {

        p <- p +
          geom_bar(aes(x=get(pivots[i]), y=avgVal),
                   fill='skyblue2', stat='identity', alpha=.8)

        if (class(dat[[pivots[i]]])!='character') {

          p <- p +
            geom_smooth(
              aes(x=as.numeric(get(pivots[i])), y=avgVal),
              method='lm', se=F, color='black', linetype='dashed', size=.75
              )

        }

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
    select(!!outcome, !!pivots, weight) %>%
    left_join(outcomeLookup)

  importantPivots <- rfPivotSelection(thisQuestionSet, outcome, pivots)

  freqPlot <- getFrequencyPlot(thisQuestionSet, recoded, mapTo, outcome)

  pivPlot <- getPivotPlots(thisQuestionSet, importantPivots)
  groupPivPlot <- getPivotPlots(thisQuestionSet, importantPivots, 'Group')

  analysis <- list(freqPlot, pivPlot, groupPivPlot)

  return(analysis)

}

selectMultiCorPlot <- function(set, questionSuffix) {

  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }

  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }

  forCorr <- set %>% select(starts_with(questionSuffix))
  names(forCorr) <- gsub(questionSuffix, '', names(forCorr))

  forCorr <-
    forCorr %>%
    cor(.)

  corPlotDat <-
    forCorr %>%
    reorder_cormat(.) %>%
    get_upper_tri(.) %>%
    melt(., na.rm=T)

  corPlot <-
    ggplot(corPlotDat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +
    coord_fixed() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      ) +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))

  corDat <-
    forCorr %>%
    melt() %>%
    filter(value < 1) %>%
    group_by(Var1) %>%
    filter(value == max(value)) %>%
    setNames(c('variable','top_cor','cor'))

  out <- list(corPlot, corDat)

  return(out)

}

selectMultiOvrPlot <- function(dat, questionSuffix, corDat) {

  ovr <-
    dat %>%
    select(starts_with(questionSuffix)) %>%
    summarise_all(funs(weighted.mean(.,weight=weight))) %>%
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

  return(ovr.p)

}

styleSelectMultiple <- function(dat, questionSuffix, pivots) {
  require(reshape2)

  pivots <- enquo(pivots)

  thisQuestionSet <-
    dat %>%
    select(!!pivots, weight) %>%
    bind_cols(dat %>% select(starts_with(questionSuffix))) %>%
    select(-ends_with('Other'), -ends_with('None_of_these')) %>%
    mutate(
      outcome = rowSums(select(., starts_with(questionSuffix)))
    )

  corOut <- selectMultiCorPlot(thisQuestionSet, questionSuffix)
  corPlot <- corOut[[1]]
  corDat <- corOut[[2]]

  ovr.p <- selectMultiOvrPlot(thisQuestionSet, questionSuffix, corDat)

  keyPivots <- rfPivotSelection(thisQuestionSet, quo(outcome), pivots)

  out <- list(corPlot, ovr.p)

  return(out)

}

