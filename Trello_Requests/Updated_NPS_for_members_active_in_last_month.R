library(tidyverse)
library(data.table)
library(broom)
library(gmodels)
library(MASS)
library(plyr)

source('config/init.R')
source('config/mySQLConfig.R')

###########################################################################
###########################################################################

###################################
#########DATA PREPPING#############

###########################################################################
###########################################################################

#Import Type form data#
Niche_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Niche 12.1.2017.csv")
Niche_typeform$survey<-"niche"
Niche_typeform$age<-"n/a"
NonNiche_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Non Niche 12.01.2017.csv")
NonNiche_typeform$survey<-"Nonniche"
NonNiche_typeform$age<-"n/a"
SMS_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/SMS only 12.1.2017.csv")
SMS_typeform$survey<-"sms"

#Rename Niche and Non-Niche columns
desiredColnames <- c('typeform_id',
                     'nps',
                     'nps_reason',
                     'ds_value',
                     'ds_value_other',
                     'causes',
                     'communication',
                     'website',
                     'scholarships',
                     'impact',
                     'community',
                     'community_3reason',
                     'community_4reason',
                     'community_5reason',
                     'important',
                     'caring',
                     'cute',
                     'edgy',
                     'cool',
                     'fun',
                     'meaningful',
                     'young',
                     'nice',
                     'exclusive',
                     'original',
                     'innovative',
                     'political',
                     'open minded',
                     'trusted',
                     'sophisticated',
                     'humurous',
                     'knowledgeable',
                     'helpful',
                     'controversial',
                     'easy',
                     'impactful',
                     'inspiring',
                     'word_other',
                     'reengage',
                     'northstar_id',
                     'reg_source',
                     'start_date',
                     'submit_date',
                     'network_id',
                     'survey',
                     'age')

#Rename SMS variables
desiredColnames_sms <- c('typeform_id',
                         'nps',
                         'nps_reason',
                         'ds_value',
                         'ds_value_other',
                         'causes',
                         'communication',
                         'website',
                         'scholarships',
                         'impact',
                         'community',
                         'community_3reason',
                         'community_4reason',
                         'community_5reason',
                         'important',
                         'caring',
                         'cute',
                         'edgy',
                         'cool',
                         'fun',
                         'meaningful',
                         'young',
                         'nice',
                         'exclusive',
                         'original',
                         'innovative',
                         'political',
                         'open minded',
                         'trusted',
                         'sophisticated',
                         'humurous',
                         'knowledgeable',
                         'helpful',
                         'controversial',
                         'easy',
                         'impactful',
                         'inspiring',
                         'word_other',
                         'reengage',
                         'age',
                         'northstar_id',
                         'reg_source',
                         'start_date',
                         'submit_date',
                         'network_id',
                         'survey'
)

#Set names for Niche and Non-Niche surveys
for (i in 1:length(colnames(Niche_typeform))) {
  colnames(Niche_typeform)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(NonNiche_typeform))) {
  colnames(NonNiche_typeform)[i] <- desiredColnames[i]
}

Niche_typeform %>%
  setNames(desiredColnames)

NonNiche_typeform%>%
  setNames(desiredColnames)

#Set names for SMS survey
for (i in 1:length(colnames(SMS_typeform))) {
  colnames(SMS_typeform)[i] <- desiredColnames_sms[i]
}

SMS_typeform %>%
  setNames(desiredColnames_sms)


#Merge all datasets
all<-rbind(Niche_typeform,NonNiche_typeform,SMS_typeform)

#create NPS categories
all<-all %>%
  mutate(
    nps_cat =
      case_when(nps<7 ~ 'Detractor',
                nps %in% c(7,8) ~ 'Persuadable',
                nps>8 ~ 'Promoter')
  )

#NPS for members who were active one month before
monthbefore<-read.csv('~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Q4 Members who were active in 01 before.csv')
monthbefore %<>%
  mutate(month=1)

#Merge with NPS Sentiment survey
merged_month<-merge(x=all, y=monthbefore, by ="northstar_id", all=TRUE)

CrossTable(merged_month$month, merged_month$nps_cat, prop.c=TRUE, prop.r=TRUE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

write.csv(merged_month, file = 'Q4 NPS.csv')
