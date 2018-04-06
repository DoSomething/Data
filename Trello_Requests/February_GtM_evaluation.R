##trello request https://trello.com/c/lzrxwBvm/1243-february-grab-the-mic-evaluation

library(tidyverse)
library(data.table)
library(rmarkdown)
library(plyr)
library(gmodels)

#Upload CSV files
control<-read.csv('~/Documents/Grab the Mic/February Typeform/GtM February Racial Justice - Control.csv')
control$group<-"control"
experiment<-read.csv('~/Documents/Grab the Mic/February Typeform/GtM February Racial Justice - Experiment.csv')
experiment$group<-"experiment"

#Rename control columns
desiredColnames <- c('Typeform_id',
                      'White',
                     'Hispanic',
                     'Black',
                     'American_Indian',
                     'Asian',
                     'Pacific_islander',
                     'Prefer_not',
                     'Other',
                     'gtm_engage_blog',
                     'gtm_engage_ama',
                     'gtm_engage_none',
                     'gtm_other',
                     'motivated',
                     'hopeful_hollywood',
                     'hopeful_officials',
                     'resources',
                     'improve_rep',
                     'racial_issues',
                     'id',
                     'Start Date (UTC)',
                     'Submit Date (UTC)',
                     'Network ID',
                     'group')

#Set names for control
for (i in 1:length(colnames(control))) {
  colnames(control)[i] <- desiredColnames[i]
}

#Rename experiment columns
desiredColnames_e <- c('Typeform_id',
                       'White',
                       'Hispanic',
                       'Black',
                       'American_Indian',
                       'Asian',
                       'Pacific_islander',
                       'Prefer_not',
                       'Other',
                       'gtm_action_poster',
                       'gtm_action_facts',
                       'gtm_action_shareguide',
                       'gtm_action_vote',
                       'gtm_action_nominate',
                       'gtm_action_none',
                       'gtm_action_other',
                       'gtm_engage_blog',
                       'gtm_engage_ama',
                       'gtm_engage_none',
                       'gtm_other',
                       'motivated',
                       'hopeful_hollywood',
                       'hopeful_officials',
                       'resources',
                       'gtm_involvement',
                       'reason_gtm',
                       'Suggestions',
                       'id',
                       'Start Date (UTC)',
                       'Submit Date (UTC)',
                       'Network ID',
                       'group')

#Set names for experiment
for (i in 1:length(colnames(experiment))) {
  colnames(experiment)[i] <- desiredColnames_e[i]
}

#select only variables that are the same as in control
colnames(experiment)
experiment_compare<-experiment[-c(1, 10:16,25:27)]

#select only variables that are the same as experiment
colnames(control)
control_compare<-control[-c(1, 18:19)]

#merge csv files
gtm_feb<-rbind(control_compare,experiment_compare)

#I feel motivated to do something about racial injustice x group
CrossTable(gtm_feb$group, gtm_feb$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$motivated~gtm_feb$group)

CrossTable(gtm_feb$gtm_motivated, gtm_feb$gtm_engage_blog, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))


#I feel hopeful about the future of Hollywood and seeing more racially inclusive casts
CrossTable(gtm_feb$group, gtm_feb$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_hollywood~gtm_feb$group)

#I feel hopeful about the future of seeing more racially diverse elected officials
CrossTable(gtm_feb$group, gtm_feb$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_officials~gtm_feb$group)

#I have the tools, resources, and knowledge to feel confident to voice my opinions on racial justice
CrossTable(gtm_feb$group, gtm_feb$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$resources~gtm_feb$group)



#Black vs non Black respondents
#Motivated - statistically significant
CrossTable(gtm_feb$Black, gtm_feb$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$motivated~gtm_feb$Black)
#Hollywood -Not statistically significant
CrossTable(gtm_feb$Black, gtm_feb$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_hollywood~gtm_feb$Black)
#Officials - Not statistically significant
CrossTable(gtm_feb$Black, gtm_feb$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_officials~gtm_feb$Black)
#Resources - Not statistically significant
CrossTable(gtm_feb$Black, gtm_feb$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$resources~gtm_feb$Black)


#Hispanic vs. other
#Motivated - statistically significant (for average)
CrossTable(gtm_feb$Hispanic, gtm_feb$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$motivated~gtm_feb$Hispanic)
#Hollywood -Not statistically significant
CrossTable(gtm_feb$Black, gtm_feb$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_hollywood~gtm_feb$Hispanic)
#Officials - statistically significant (for average)
CrossTable(gtm_feb$Hispanic, gtm_feb$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_officials~gtm_feb$Hispanic)
#Resources - Not statistically significant
CrossTable(gtm_feb$Hispanic, gtm_feb$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$resources~gtm_feb$Hispanic)


#Asian vs. other
#Motivated - Not statistically significant
CrossTable(gtm_feb$Asian, gtm_feb$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$motivated~gtm_feb$Asian)
#Hollywood -Not statistically significant
CrossTable(gtm_feb$Asian, gtm_feb$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_hollywood~gtm_feb$Asian)
#Officials - Not statistically significant
CrossTable(gtm_feb$Asian, gtm_feb$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_officials~gtm_feb$Asian)
#Resources - Not statistically significant
CrossTable(gtm_feb$Asian, gtm_feb$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$resources~gtm_feb$Asian)

#White vs. other
#Motivated - Not statistically significant
CrossTable(gtm_feb$White, gtm_feb$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$motivated~gtm_feb$White)
#Hollywood -Not statistically significant
CrossTable(gtm_feb$White, gtm_feb$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_hollywood~gtm_feb$White)
#Officials - Not statistically significant
CrossTable(gtm_feb$White, gtm_feb$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$hopeful_officials~gtm_feb$White)
#Resources - Not statistically significant
CrossTable(gtm_feb$White, gtm_feb$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb$resources~gtm_feb$White)

#GtM Actions
#Only look at experiment group
gtm_feb_experiment<-gtm_feb%>%
  filter(group=='experiment')

#Recode for didn't read blog
gtm_feb_experiment<-experiment%>%
  mutate(noblog=
           ifelse(gtm_engage_blog=='',1,0),
         noama=
           ifelse(gtm_engage_ama=='',1,0),
         none=
           ifelse(gtm_engage_none=='',1,0),
         noposter=
           ifelse(gtm_action_poster=='',1,0),
         novote=
           ifelse(gtm_action_vote=='',1,0),
         noguide=
           ifelse(gtm_action_shareguide=='',1,0),
         nofacts=
           ifelse(gtm_action_facts=='',1,0),
         nonominate=
           ifelse(gtm_action_nominate=='',1,0))

#Blog
CrossTable(gtm_feb_experiment$noblog, gtm_feb_experiment$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$motivated~gtm_feb_experiment$noblog)

CrossTable(gtm_feb_experiment$noblog, gtm_feb_experiment$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_hollywood~gtm_feb_experiment$noblog)

CrossTable(gtm_feb_experiment$noblog, gtm_feb_experiment$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_officials~gtm_feb_experiment$noblog)

CrossTable(gtm_feb_experiment$noblog, gtm_feb_experiment$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$resources~gtm_feb_experiment$noblog)


#Ama
CrossTable(gtm_feb_experiment$noama, gtm_feb_experiment$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$motivated~gtm_feb_experiment$noama)

CrossTable(gtm_feb_experiment$noama, gtm_feb_experiment$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_hollywood~gtm_feb_experiment$noama)

CrossTable(gtm_feb_experiment$noama, gtm_feb_experiment$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_officials~gtm_feb_experiment$noama)

CrossTable(gtm_feb_experiment$noama, gtm_feb_experiment$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$resources~gtm_feb_experiment$noama)

#Poster
CrossTable(gtm_feb_experiment$noposter, gtm_feb_experiment$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$motivated~gtm_feb_experiment$noposter)


#Encouraged others to vote
CrossTable(gtm_feb_experiment$novote, gtm_feb_experiment$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$motivated~gtm_feb_experiment$novote)

CrossTable(gtm_feb_experiment$novote, gtm_feb_experiment$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_hollywood~gtm_feb_experiment$novote)

CrossTable(gtm_feb_experiment$novote, gtm_feb_experiment$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_officials~gtm_feb_experiment$novote)

CrossTable(gtm_feb_experiment$novote, gtm_feb_experiment$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$resources~gtm_feb_experiment$novote)

#Shared facts about voter suppression
CrossTable(gtm_feb_experiment$nofacts, gtm_feb_experiment$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$motivated~gtm_feb_experiment$nofacts)

CrossTable(gtm_feb_experiment$nofacts, gtm_feb_experiment$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_hollywood~gtm_feb_experiment$nofacts)

CrossTable(gtm_feb_experiment$nofacts, gtm_feb_experiment$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_officials~gtm_feb_experiment$nofacts)

CrossTable(gtm_feb_experiment$nofacts, gtm_feb_experiment$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$resources~gtm_feb_experiment$nofacts)

#Shared Netflix Guide
CrossTable(gtm_feb_experiment$noguide, gtm_feb_experiment$motivated, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$motivated~gtm_feb_experiment$noguide)

CrossTable(gtm_feb_experiment$noguide, gtm_feb_experiment$hopeful_hollywood, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_hollywood~gtm_feb_experiment$noguide)

CrossTable(gtm_feb_experiment$noguide, gtm_feb_experiment$hopeful_officials, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$hopeful_officials~gtm_feb_experiment$noguide)

CrossTable(gtm_feb_experiment$noguide, gtm_feb_experiment$resources, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
t.test(gtm_feb_experiment$resources~gtm_feb_experiment$noguide)

