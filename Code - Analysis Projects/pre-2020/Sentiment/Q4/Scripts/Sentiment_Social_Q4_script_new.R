library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate)
library(magrittr)
library(MASS)
library(gmodels)
library(QuantPsyc)


social<-read.xlsx('~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/All social merged.xlsx')

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
                     'prizes',
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

social_all<-social %>%
  setNames(desiredColnames)


social_nps<-social_all %>%
  mutate(
    nps_cat =
      case_when(nps<7 ~ 'Detractor',
                nps %in% c(7,8) ~ 'Persuadable',
                nps>8 ~ 'Promoter')
  )

write.csv(social_nps, file = "Q4 Sentiment Social.csv")

ddply(social_nps, summarize, causes_mean=mean(causes), communication_mean=mean(communication),
      website_mean=mean(website), impact_mean=mean(impact), community_mean=mean(community),
      scholarships_mean=mean(scholarships))

CrossTable(social_nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(social_nps$important, social_nps$nps_cat, prop.c=TRUE, prop.r=TRUE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
