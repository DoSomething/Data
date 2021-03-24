# Init --------------------------------------------------------------------
library(rpart)
library(rpart.plot)
library(caret)
library(ROCR)
source('Scripts/init.R')
source('Scripts/DSUtils.R')

# Prep Data ---------------------------------------------------------------
prepControl <- function(path) {
  ctdControl <- 
    read_csv(path) %>%
    tbl_dt() %>%
    setNames(
      c('timestamp','gender','geography','licenseType','driveFrequency','intervenedFriend',
        'intervenedFamily','textDriveFreq','textDriveDanger','email')
    ) %>%
    mutate(
      group = 'Control'
    )
  return(ctdControl)
}

prep4_8 <- function(path) {
  ctd4_8 <- 
    read_csv(path) %>%
    tbl_dt() %>%
    setNames(
      c('timestamp','gender','geography','licenseType','driveFrequency','intervenedFriend',
        'intervenedFamily','textDriveFreq','textDriveDanger','takeAction')
    ) %>%
    mutate(
      group = '4-8 Weeks'
    )
  return(ctd4_8)
}

prep72 <- function(path) {
  
  ctd72 <- 
    read_csv(path) %>%
    tbl_dt() %>% 
    setNames(
      c('timestamp','gender','geography','licenseType','driveFrequency','intervenedFriend',
        'intervenedFamily','textDriveFreq','textDriveDanger','ctdStrategies','ctd.willInterveneFriend',
        'ctd.willInterveneFamily','takeAction')
    ) %>%
    mutate(
      group = '72 Hours'
    )
  
  return(ctd72)
}

prepData <- function(pathControl, pathExp1, pathExp2) {
  control <- prepControl(pathControl)
  ctd4_8 <- prep4_8(pathExp1)
  ctd72 <- prep72(pathExp2)
  
  ctd <- 
    rbind(control, ctd4_8, ctd72, fill=T) %>%
    mutate(
      date = as.Date(substr(timestamp, 1,10), format='%Y/%m/%d')
    ) %>%
    tbl_dt()
  
  return(ctd)
  
}

ctd <- 
  prepData(
    'Data/Crash Text Dummy - Control.csv', 
    'Data/Crash Text Dummy - 4-8 Weeks.csv', 
    'Data/Crash Text Dummy - 72 Hours.csv'
    )

table(ctd$textDrive)
ctd %<>%
  mutate(
    textDrive.yn = ifelse(textDriveFreq=="I don't drive", NA, 
                                  ifelse(textDriveFreq=='Never', 0, 1)),
    textDrive = factor(ifelse(grepl('Only', textDriveFreq), 'Sometimes', textDriveFreq), levels=c('Often','Sometimes','Never')),
    licenseType = ifelse(licenseType=="Driver's license", 'License', 
                         ifelse(licenseType=="Driver's permit", 'Permit', 'Neither')),
    gender = ifelse(gender=='Man', 'Male', 
                    ifelse(gender=='Woman', 'Female', 'Other/DNR')),
    intervenedFriend = ifelse(!(intervenedFriend %in% c('No','Yes')), 'NotApplicable', intervenedFriend),
    intervenedFamily = ifelse(!(intervenedFriend %in% c('No','Yes')), 'NotApplicable', intervenedFamily),
    group = factor(group, levels=c('Control','72 Hours','4-8 Weeks')),
    timePeriod = ifelse(date < '2017-03-21', 'Before', 'After')
  )

# Difference in Difference -----------------------------------------------------------

ggplot(ctd, aes(x=group,y=date)) + 
  geom_violin(position='dodge',aes(fill=group)) + 
  coord_flip() + theme(legend.position="none") + 
  ggtitle('Survey Date by Group') + 
  theme(plot.title = element_text(hjust = 0.5))

DnD <- function(data, pivot) {
  
  DnD <-
    data %>%
    filter(!is.na(get(pivot))) %>%
    group_by(group, timePeriod) %>%
    summarise(
      Frequency = mean(get(pivot))
    ) %>%
    spread(timePeriod, Frequency, fill=ctd[group=='Control' & timePeriod=='Before',mean(get(pivot), na.rm=T)]) %>%
    select(group, Before, After) %>%
    mutate(
      Diff = After - Before
    ) 
  DnD <-
    rbind(
      DnD,
      data.frame(group='Effect - 72 Hours',Before=NA,After=NA,Diff=DnD[group=='72 Hours',Diff]-DnD[group=='Control',Diff]),
      data.frame(group='Effect - 4-8 Weeks',Before=NA,After=NA,Diff=DnD[group=='4-8 Weeks',Diff]-DnD[group=='Control',Diff])
    )
  
}

textDrive.DnD <- DnD(ctd, 'textDrive.yn') %>% print()

# Proportions -------------------------------------------------------------

proportionPivot <- function(data, pivot=NULL) {
  if(is.null(pivot)) { 
    outcomeFreq <-
      ctd %>%
      as_tibble() %>%
      filter(textDrive != "I don't drive") %>%
      group_by_('textDrive', 'group') %>%
      summarise(
        count = n()
      ) %>%
      left_join(
        ctd %>% as_tibble %>% filter(textDrive != "I don't drive") %>% group_by_('group') %>% summarise(countGroup = n())
      ) %>%
      mutate(
        proportion = count / countGroup
      ) %>%
      tbl_dt()
  } else {
    outcomeFreq <-
      ctd %>%
      as_tibble() %>%
      filter(textDrive != "I don't drive") %>%
      group_by_('textDrive', 'group', pivot) %>%
      summarise(
        count = n()
      ) %>%
      left_join(
        ctd %>% as_tibble %>% filter(textDrive != "I don't drive") %>% group_by_('group', pivot) %>% summarise(countGroup = n())
      ) %>%
      mutate(
        proportion = count / countGroup
      ) %>%
      tbl_dt()
  }
  
  return(outcomeFreq)
  
}

proportionPlot <- function(data, wrap, title) {
  plot <- ggplot(data, aes(x=group, y=proportion, group=textDrive)) + 
    geom_bar(stat='identity',position='stack',aes(fill=textDrive)) + facet_wrap(~get(wrap)) + 
    scale_y_continuous(breaks = pretty_breaks(n=10)) + scale_y_continuous(breaks = pretty_breaks(n=20)) +s
    theme(plot.title = element_text(hjust = 0.5)) + ggtitle(paste(title))
  return(plot)
}

ggplot(proportionPivot(ctd), aes(x=group,y=proportion,group=textDrive)) + 
  geom_bar(stat='identity',position='stack',aes(fill=textDrive)) +
  scale_y_continuous(breaks = pretty_breaks(n=10)) +
  theme(plot.title = element_text(hjust = 0.5)) + ggtitle('Overall')

proportionsLicenseType <- 
  proportionPivot(ctd, 'licenseType') %>%
  proportionPlot('licenseType', 'License Type') %>%
  print()

proportionsGender <- 
  proportionPivot(ctd, 'gender') %>%
  proportionPlot('gender', 'Gender') %>%
  print()

proportionsDriveFrequncy <- 
  proportionPivot(ctd, 'driveFrequency') %>%
  proportionPlot('driveFrequency', 'Drive Frequency') %>%
  print()

proportionsGeography <- 
  proportionPivot(ctd, 'geography') %>%
  proportionPlot('geography', 'Geography') %>%
  print()


# Capture Important Effects -----------------------------------------------

control <- rpart(
  as.factor(textDrive.yn) ~ 
    gender + geography + driveFrequency + licenseType, 
  data = ctd[group=='Control' & !is.na(textDrive.yn)]
  , cp=0.01)
rpart.plot(control, main="Control")

exp <- rpart(
  as.factor(textDrive.yn) ~ 
    group + gender + geography + driveFrequency + licenseType, 
  data = ctd[group!='Control']
  , cp=0.01) 
rpart.plot(exp, main="Experiment")

ctrl = trainControl(method="repeatedcv", number=10, repeats=5, selectionFunction = "oneSE", 
                    classProb=T, summaryFunction = twoClassSummary, savePredictions = TRUE)

ctd %<>% mutate(textDrive.rf = as.factor(ifelse(textDrive.yn == 1, 'yes', 'no')))

candidates = c('gender', 'geography', 'driveFrequency', 'licenseType', 'group')
formula = as.formula(paste('textDrive.rf ~ ',paste(candidates, sep = ' + ', collapse = '+')))

rf = train(
  formula, 
  data=ctd[!is.na(textDrive.yn)], 
  method="rf", 
  metric="ROC",
  trControl=ctrl
)

varImp(rf, scale=FALSE) %>% plot(main = 'Variable Importance')
print(paste0('AUC = ', 1-round(rf$results$Spec[1], 4)))

# Estimate Important Effects ----------------------------------------------

remainingCandidates <- c('geography', 'driveFrequency', 'licenseType', 'group')
formula = as.formula(paste('textDrive.yn ~ ',paste(remainingCandidates, sep = ' + ', collapse = '+')))

linMod <- glm(formula, 'binomial', data = ctd[!is.na(textDrive.yn)])

finalCandidates <- c('driveFrequency', 'licenseType', 'group')
formula = as.formula('textDrive.yn ~ driveFrequency + licenseType + group + group*licenseType')

linMod <- glm(formula, 'binomial', data = ctd[!is.na(textDrive.yn)])

effects <- 
  data.table(
  Name = names(coef(summary(linMod))[,1]), 
  Coef = coef(summary(linMod))[,1], 
  SE = coef(summary(linMod))[,2],
  pVal = round(coef(summary(linMod))[,4], 4)
  ) %>%
  tbl_dt() %>%
  filter(Name != '(Intercept)') %>%
  mutate(
    effectSize = logit2prob(Coef)
  )

Probs <- 
  expand.grid(
    driveFrequency = unique(ctd[!is.na(textDrive.yn), driveFrequency]),
    licenseType = unique(ctd[!is.na(textDrive.yn), licenseType]),
    group = unique(ctd[!is.na(textDrive.yn), group])
  ) %>%
  tbl_dt()
Probs %<>% mutate(pTextDrive = predict(linMod, Probs, type='response'))

ggplot(Probs, aes(x=driveFrequency, y=pTextDrive, group=group)) + 
  geom_bar(stat='identity', position='dodge', aes(fill=group)) + 
  facet_wrap(~licenseType) + 
  labs(x='Driving Frequency', y='Likelihood to Text & Drive', title='Campaign Effectiveness') + 
  theme(plot.title = element_text(hjust = 0.5))

probTable <- 
  Probs %>%
  spread(
    group, pTextDrive
  )
