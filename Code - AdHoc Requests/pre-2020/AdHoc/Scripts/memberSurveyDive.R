source('Scripts/init.R')
source('Scripts/DSUtils.R')
library(foreign)

mem2016 <- read.spss('Data/Member 2016 Survey (for Google).sav', to.data.frame = T) %>% tbl_dt()
mem2017 <- read.spss('Data/Member 2017 Survey (for Google).sav', to.data.frame = T) %>% tbl_dt()
extern2017 <- read.spss('Data/External 2016 Survey (for Google).sav', to.data.frame = T) %>% tbl_dt()

####Importance####
importance2017 <-
  mem2017 %>%
  select(Response_ID, Age, starts_with('Imp_'))

ImpCols <- importance2017 %>% select(starts_with('Imp')) %>% colnames()
importance2017 <- 
  importance2017[, (ImpCols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = ImpCols]

importance2017 <-
  mem2017 %>%
  filter(!is.na(Age) & Age < 19) %>%
  melt(id.vars = c('Response_ID', 'Age'), measure.vars = ImpCols, variable.name = 'Issue', value.name = 'Importance') %>%
  tbl_dt() %>%
  mutate(Issue = substr(as.character(Issue), 5, nchar(as.character(Issue))))

ggsave(
  ggplot(importance2017[Issue != 'Trade'], aes(x=Age, y=Importance)) + 
    geom_smooth() + 
    facet_wrap(~Issue) + scale_x_continuous(breaks=pretty_breaks(7)) +
    ggtitle('Importance of Issue by Age'),
  filename = '~/Desktop/Importance Regressions.jpeg',
  width = 10, height = 6, units = 'in'
)

####Actions####
action2017 <-
  mem2017 %>%
  select(Response_ID, Age, starts_with('Action_'))

actionCols <- action2017 %>% select(starts_with('Action_')) %>% colnames()
action2017 <- 
  action2017[
    , 
    (actionCols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
    .SDcols = actionCols
    ]

action2017 <-
  action2017 %>%
  filter(!is.na(Age) & Age < 19) %>%
  melt(id.vars = c('Response_ID', 'Age'), measure.vars = actionCols, variable.name = 'Type', value.name = 'Action') %>%
  tbl_dt() %>%
  mutate(Type = substr(as.character(Type), 8 , nchar(as.character(Type))))

ggsave(
  ggplot(action2017[Type!='Other'], aes(x=Age, y=Action)) + 
    geom_smooth() + 
    facet_wrap(~Type, scales = "free_y") + scale_x_continuous(breaks=pretty_breaks(7)) +
    ggtitle('Action Taken on Issue by Age'),
  filename = '~/Desktop/Action Regressions.jpeg',
  width = 12, height = 8, units = 'in'
)
