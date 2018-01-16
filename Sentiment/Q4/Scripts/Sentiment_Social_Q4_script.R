library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate)
library(magrittr)
library(MASS)
library(gmodels)
library(QuantPsyc)

source('config/init.R')
source('config/mySQLConfig.R')

#Import Type form data
FB_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/FB 12.4.2017.csv")
FB_typeform$survey<-"Facebook"
IG_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/IG 12.4.2017.csv")
IG_typeform$survey<-"Instagram"
Twitter_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Twitter 12.4.2017.csv")
Twitter_typeform$survey<-"Twitter"

#rename columns
desiredColnames <- c('id_typeform',
                     'age',
                     'twitter_follow',
                     'ig_follow',
                     'fb_follow',
                     'snapchat_follow',
                     'youtube_follow',
                     'tumblr_follow',
                     'other_follow',
                     'none_follow',
                     'when_follow',
                     'reason_scholarships',
                     'reason_creativeposts',
                     'reason_learnDSprojects',
                     'reason_participateDSprojects',
                     'reason_socialissues',
                     'reason_entertainingposts',
                     'reason_contactDS',
                     'reason_reflectsvalues',
                     'reason_internships',
                     'reason_other',
                     'ds_value',
                     'ds_value_other',
                     'causes',
                     'posts',
                     'impact',
                     'community',
                     'community_3',
                     'community_4',
                     'community_5',
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
                     'nps',
                     'nps_reason',
                     'DSemailtext',
                     'DSemailtext_when',
                     'DScontact_email',
                     'DScontact_either',
                     'start_date',
                    'submit_date',
                    'network_id',
                    'survey')

# for (i in 1:length(colnames(SMS_typeform))) {
#   colnames(SMS_typeform)[i] <- desiredColnames[i]
# }

FB_typeform2<-FB_typeform %>%
  setNames(desiredColnames)

write.csv(FB_typeform2, file = "Q4 FB Sentiment2.csv")
