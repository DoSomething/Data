library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate)
library(magrittr)

source('config/init.R')
source('config/mySQLConfig.R')

#Import Type form data
Niche_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Niche 11.29.2017.csv")
Niche_typeform$survey<-"niche"
Niche_typeform$age<-"n/a"
NonNiche_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Non Niche 11.29.2017.csv")
NonNiche_typeform$survey<-"Nonniche"
NonNiche_typeform$age<-"n/a"
SMS_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/SMS only 11.29.2017.csv")
SMS_typeform$survey<-"sms"

#Rename Niche variables
colnames(Niche_typeform)[1] <-"id_typeform"
colnames(Niche_typeform)[2] <-"nps"
colnames(Niche_typeform)[3] <-"nps_reason"
colnames(Niche_typeform)[4] <-"ds_value"
colnames(Niche_typeform)[5] <-"ds_value_other"
colnames(Niche_typeform)[6] <-"causes"
colnames(Niche_typeform)[7] <-"communication"
colnames(Niche_typeform)[8] <-"website"
colnames(Niche_typeform)[9] <-"scholarships"
colnames(Niche_typeform)[10] <-"impact"
colnames(Niche_typeform)[11] <-"community"
colnames(Niche_typeform)[12] <-"community_3reason"
colnames(Niche_typeform)[13] <-"community_4reason"
colnames(Niche_typeform)[14] <-"community_5reason"
colnames(Niche_typeform)[15] <-"important"
colnames(Niche_typeform)[16] <-"caring"
colnames(Niche_typeform)[17] <-"cute"
colnames(Niche_typeform)[18] <-"edgy"
colnames(Niche_typeform)[19] <-"cool"
colnames(Niche_typeform)[20] <-"fun"
colnames(Niche_typeform)[21] <-"meaningful"
colnames(Niche_typeform)[22] <-"young"
colnames(Niche_typeform)[23] <-"nice"
colnames(Niche_typeform)[24] <-"exclusive"
colnames(Niche_typeform)[25] <-"original"
colnames(Niche_typeform)[26] <-"innovative"
colnames(Niche_typeform)[27] <-"political"
colnames(Niche_typeform)[28] <-"open minded"
colnames(Niche_typeform)[29] <-"trusted"
colnames(Niche_typeform)[30] <-"sophisticated"
colnames(Niche_typeform)[31] <-"humurous"
colnames(Niche_typeform)[32] <-"knowledgeable"
colnames(Niche_typeform)[33] <-"helpful"
colnames(Niche_typeform)[34] <-"controversial"
colnames(Niche_typeform)[35] <-"easy"
colnames(Niche_typeform)[36] <-"impactful"
colnames(Niche_typeform)[37] <-"inspiring"
colnames(Niche_typeform)[38] <-"word_other"
colnames(Niche_typeform)[39] <-"reengage"
colnames(Niche_typeform)[40] <-"northstar_id"
colnames(Niche_typeform)[41] <-"reg_source"
colnames(Niche_typeform)[42] <-"start_date"
colnames(Niche_typeform)[43] <-"submit_date"
colnames(Niche_typeform)[44] <-"network_id"

#Rename Non-Niche variables
colnames(NonNiche_typeform)[1] <-"id_typeform"
colnames(NonNiche_typeform)[2] <-"nps"
colnames(NonNiche_typeform)[3] <-"nps_reason"
colnames(NonNiche_typeform)[4] <-"ds_value"
colnames(NonNiche_typeform)[5] <-"ds_value_other"
colnames(NonNiche_typeform)[6] <-"causes"
colnames(NonNiche_typeform)[7] <-"communication"
colnames(NonNiche_typeform)[8] <-"website"
colnames(NonNiche_typeform)[9] <-"scholarships"
colnames(NonNiche_typeform)[10] <-"impact"
colnames(NonNiche_typeform)[11] <-"community"
colnames(NonNiche_typeform)[12] <-"community_3reason"
colnames(NonNiche_typeform)[13] <-"community_4reason"
colnames(NonNiche_typeform)[14] <-"community_5reason"
colnames(NonNiche_typeform)[15] <-"important"
colnames(NonNiche_typeform)[16] <-"caring"
colnames(NonNiche_typeform)[17] <-"cute"
colnames(NonNiche_typeform)[18] <-"edgy"
colnames(NonNiche_typeform)[19] <-"cool"
colnames(NonNiche_typeform)[20] <-"fun"
colnames(NonNiche_typeform)[21] <-"meaningful"
colnames(NonNiche_typeform)[22] <-"young"
colnames(NonNiche_typeform)[23] <-"nice"
colnames(NonNiche_typeform)[24] <-"exclusive"
colnames(NonNiche_typeform)[25] <-"original"
colnames(NonNiche_typeform)[26] <-"innovative"
colnames(NonNiche_typeform)[27] <-"political"
colnames(NonNiche_typeform)[28] <-"open minded"
colnames(NonNiche_typeform)[29] <-"trusted"
colnames(NonNiche_typeform)[30] <-"sophisticated"
colnames(NonNiche_typeform)[31] <-"humurous"
colnames(NonNiche_typeform)[32] <-"knowledgeable"
colnames(NonNiche_typeform)[33] <-"helpful"
colnames(NonNiche_typeform)[34] <-"controversial"
colnames(NonNiche_typeform)[35] <-"easy"
colnames(NonNiche_typeform)[36] <-"impactful"
colnames(NonNiche_typeform)[37] <-"inspiring"
colnames(NonNiche_typeform)[38] <-"word_other"
colnames(NonNiche_typeform)[39] <-"reengage"
colnames(NonNiche_typeform)[40] <-"northstar_id"
colnames(NonNiche_typeform)[41] <-"reg_source"
colnames(NonNiche_typeform)[42] <-"start_date"
colnames(NonNiche_typeform)[43] <-"submit_date"
colnames(NonNiche_typeform)[44] <-"network_id"

#Rename SMS variables
colnames(SMS_typeform)[1] <-"id_typeform"
colnames(SMS_typeform)[2] <-"nps"
colnames(SMS_typeform)[3] <-"nps_reason"
colnames(SMS_typeform)[4] <-"ds_value"
colnames(SMS_typeform)[5] <-"ds_value_other"
colnames(SMS_typeform)[6] <-"causes"
colnames(SMS_typeform)[7] <-"communication"
colnames(SMS_typeform)[8] <-"website"
colnames(SMS_typeform)[9] <-"scholarships"
colnames(SMS_typeform)[10] <-"impact"
colnames(SMS_typeform)[11] <-"community"
colnames(SMS_typeform)[12] <-"community_3reason"
colnames(SMS_typeform)[13] <-"community_4reason"
colnames(SMS_typeform)[14] <-"community_5reason"
colnames(SMS_typeform)[15] <-"important"
colnames(SMS_typeform)[16] <-"caring"
colnames(SMS_typeform)[17] <-"cute"
colnames(SMS_typeform)[18] <-"edgy"
colnames(SMS_typeform)[19] <-"cool"
colnames(SMS_typeform)[20] <-"fun"
colnames(SMS_typeform)[21] <-"meaningful"
colnames(SMS_typeform)[22] <-"young"
colnames(SMS_typeform)[23] <-"nice"
colnames(SMS_typeform)[24] <-"exclusive"
colnames(SMS_typeform)[25] <-"original"
colnames(SMS_typeform)[26] <-"innovative"
colnames(SMS_typeform)[27] <-"political"
colnames(SMS_typeform)[28] <-"open minded"
colnames(SMS_typeform)[29] <-"trusted"
colnames(SMS_typeform)[30] <-"sophisticated"
colnames(SMS_typeform)[31] <-"humurous"
colnames(SMS_typeform)[32] <-"knowledgeable"
colnames(SMS_typeform)[33] <-"helpful"
colnames(SMS_typeform)[34] <-"controversial"
colnames(SMS_typeform)[35] <-"easy"
colnames(SMS_typeform)[36] <-"impactful"
colnames(SMS_typeform)[37] <-"inspiring"
colnames(SMS_typeform)[38] <-"word_other"
colnames(SMS_typeform)[39] <-"reengage"
colnames(SMS_typeform)[40] <-"age"
colnames(SMS_typeform)[41] <-"northstar_id"
colnames(SMS_typeform)[42] <-"reg_source"
colnames(SMS_typeform)[43] <-"start_date"
colnames(SMS_typeform)[44] <-"submit_date"
colnames(SMS_typeform)[45] <-"network_id"

#merge all datasets
all<-rbind(Niche_typeform,NonNiche_typeform,SMS_typeform)

#create NPS categories
setDT(all)[nps<7, nps_cat:= "Detractor"]
all[nps==7|nps==8, nps_cat:= "Persuadable"]
all[nps>8, nps_cat:= "Promoter"]

write.csv(all, file = "all.csv")

##Merge with db data
all_copy<-all
a<-prepQueryObjects(all_copy$northstar_id)

#Query to pull db data and then link to members who completed NPS survey
q<- paste0("
          SELECT ca.northstar_id,
            u.northstar_created_at_timestamp,
            u.last_logged_in,
            u.last_accessed,
            u.northstar_id_source_name,
            u.email,
            u.mobile,
            u.facebook_id,
            extract(year FROM (from_days(dateDiff(current_timestamp,u.birthdate)))) AS age,
            Year(now()) - Year(u.northstar_created_at_timestamp) as 'years_a_member',
            count(DISTINCT ca.signup_id) AS total_signups,
            count(DISTINCT ca.quantity) AS total_rbs,
            count(DISTINCT ul.last_accessed) as 'site_visits'
          FROM quasar.users u
          LEFT JOIN quasar.campaign_activity ca
          ON u.northstar_id=ca.northstar_id
          LEFT JOIN quasar.users_log ul
          ON u.northstar_id=ul.northstar_id
          WHERE u.northstar_id IN", a,
          "GROUP BY u.northstar_id")

qres <- runQuery(q, which = 'mysql')

#Query for action types
action_types<- paste0("
        SELECT ca.northstar_id,
          ca.campaign_run_id,
          ci.campaign_cause_type as 'cause_type',
          ci.campaign_action_type as'action_type'
        FROM quasar.campaign_activity ca
        LEFT JOIN quasar.users u
        ON ca.northstar_id=u.northstar_id
        LEFT JOIN quasar.campaign_info ci
        ON ca.campaign_run_id = ci.campaign_run_id
        WHERE ca.northstar_id IN", a,
        "GROUP BY ca.northstar_id")

qres_action <-runQuery(action_types, which ='mysql')

#merge db data with survey data
# Merged<- all %>%
#   left_join(qres, by = 'northstar_id')
merged<-merge(x=all, y=qres, by ="northstar_id", all=TRUE)

write.csv(merged, file = "all_merged.csv")


