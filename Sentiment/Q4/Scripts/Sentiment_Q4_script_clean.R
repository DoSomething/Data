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

# Alternative code to set NPS categories
#setDT(all)[nps<7, nps_cat:= "Detractor"]
# all[nps==7|nps==8, nps_cat:= "Persuadable"]
# all[nps>8, nps_cat:= "Promoter"]

write.csv(all, file = "all.csv")


###########################################################################
###########################################################################

##########LINKING SENTIMENT SURVEY WITH NSID DATABASE DATA#################

###########################################################################
###########################################################################


##Concatenate NSIDs so you can include them in SQL query as object 'a'
all_copy<-all
a<-prepQueryObjects(all_copy$northstar_id)

#Query to pull db data and then link to members who completed NPS survey
q<- paste0("
          SELECT ca.northstar_id,
            u.created_at,
            u.last_logged_in,
            u.last_accessed,
            u.source,
            u.email,
            u.mobile,
            u.facebook_id,
            MAX(ca.signup_created_at),
            extract(year FROM (from_days(dateDiff(current_timestamp,u.birthdate)))) AS age,
            Year(now()) - Year(u.created_at) as 'years_a_member',
            count(DISTINCT ca.signup_id) AS total_signups,
            sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
            max(case when greatest(u.last_accessed, u.last_logged_in) > u.created_at AND u.source='niche' THEN 1 ELSE 0 END) as Niche_activated,
            case when (count(DISTINCT ca.signup_id)>1) THEN 1 ElSE 0 END AS Niche_activated_other,
            count(DISTINCT ul.last_accessed) as 'site_visits'
          FROM quasar.users u
          LEFT JOIN quasar.campaign_activity ca
          ON u.northstar_id=ca.northstar_id
          LEFT JOIN quasar.users_log ul
          ON u.northstar_id=ul.northstar_id
          WHERE u.northstar_id IN", a,
          "GROUP BY u.northstar_id")

qres <- runQuery(q, which = 'mysql')

#Create variables for signup and rb count categories
qres<-qres %>%
  mutate(
    signups_cat =
      case_when(total_signups==1 ~ '1 signup',
                total_signups==2 ~ '2 signups',
                total_signups==3 ~ '3 signups',
                total_signups==4 ~ '4 signups',
                total_signups>4 ~ '5+ signups'),
    rbs_cat =
      case_when(total_rbs==1 ~ '1 reportbacks',
                total_rbs==2 ~ '2 reportbacks',
                total_rbs==3 ~ '3 reportbacks',
                total_rbs==4 ~ '4 reportbacks',
                total_rbs>4 ~ '5+ reportbacks')
  )

#Merge with NPS Sentiment survey
merged<-merge(x=all, y=qres, by ="northstar_id", all=TRUE)

#Create variable to identify "bad niche" (unactivated Niche members) - should be 82 members
# merged<-merged%>%
#   mutate(
#     bad_niche=ifelse(survey =='niche' & Niche_activated==0 & total_rbs==0 & total_signups<=1,1,0)
#   )
#
# #Check code for 'Bad Niche' are only in Niche
# CrossTable(merged$survey, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#
# write.csv(merged, file = "Bad Niche.csv")

#Identify "Engaged" Niche users (Q1 definition).
# b<-prepQueryObjects(previouslyUsedNSIDs$northstar_id)
# q_niche_q4<- paste0("
#                     SELECT
#                     distinct a.northstar_id
#                     , a.created_at
#                     , a.event_name
#                     , a.timestamp
#                     FROM (
#                     -- web activation
#                     SELECT
#                     u.northstar_id AS 'northstar_id'
#                     , u.created_at AS 'created_at'
#                     , 'activated' AS 'event_name'
#                     , l.last_logged_in AS 'timestamp'
#                     FROM
#                     quasar.users u
#                     LEFT JOIN
#                     quasar.users_log l
#                     ON
#                     l.northstar_id = u.northstar_id
#                     WHERE
#                     u.source = 'niche'
#                     AND l.last_logged_in <> '0000-00-00 00:00:00'
#                     AND l.last_logged_in <> '1970-01-01 00:00:00'
#                     GROUP BY
#                     u.northstar_id
#                     HAVING
#                     min(l.last_logged_in) > u.created_at
#                     UNION ALL
#                     -- sms signups
#                     SELECT
#                     u.northstar_id AS 'northstar_id'
#                     , u.created_at AS 'created_at'
#                     , 'sms_signup' AS 'event_name'
#                     , MAX(signup_created_at) AS 'timestamp'
#                     FROM
#                     quasar.users u
#                     LEFT JOIN
#                     quasar.campaign_activity c
#                     ON
#                     c.northstar_id = u.northstar_id
#                     LEFT JOIN
#                     quasar.users_log l
#                     ON
#                     l.northstar_id = u.northstar_id
#                     WHERE
#                     u.source = 'niche'
#                     GROUP BY
#                     u.northstar_id
#                     HAVING
#                     count(distinct c.signup_id) > 1
#                     AND min(l.last_logged_in) <= u.created_at
#                     ) AS a
#                     WHERE a.northstar_id IN", b,
#                     "GROUP BY
#                     a.northstar_id")
#
# qres_Q4_engagedniche <- runQuery(q_niche_q4, which = 'mysql')
#
# #merge Engaged Niche data with data.
# #Merge with NPS Sentiment survey
# merged_Q4_all<-merge(x=all, y=qres_Q4_engagedniche, by ="northstar_id", all=TRUE)
#
# merged_Q4_all<-merged_Q4_all %>%
#   mutate(
#     Niche_engaged = ifelse(event_name=='activated' | event_name=='sms_signup',1,0
#     ))



###########################################################################
###########################################################################

##############LINKING CAMPAIGN DATA TO SENTIMENT DATA #####################

###########################################################################
###########################################################################


#Query for pulling campaigns, causes, and action types for those who completed Sentiment survey
action_types<- paste0("
        SELECT DISTINCT ca.northstar_id,
          ci.campaign_node_id_title,
          ci.campaign_cause_type as 'cause_type',
          ci.campaign_action_type as'action_type'
        FROM quasar.campaign_activity ca
        LEFT JOIN quasar.campaign_info ci
        ON ca.campaign_run_id = ci.campaign_run_id
        WHERE ca.northstar_id IN", a)
#
qres_action <-runQuery(action_types, which ='mysql')

write.csv(qres_action, file = 'Q4 Campaigns.csv')

#Reshape data from short to long form and count # times action or cause done by each member
castAction <-
  qres_action%>%
  mutate(action_type = gsub(' ', '', action_type)) %>%
  dplyr::select(northstar_id, action_type) %>%
  dcast(northstar_id ~ paste0('action_type_',action_type)) %>%
  left_join(
    qres_action%>%
      mutate(cause_type = gsub(' ', '', cause_type)) %>%
      dplyr::select(northstar_id, cause_type) %>%
      dcast(northstar_id ~ paste0('cause_type_',cause_type))
  )

#Merge action/cause types with Type Form and DB data
cause_action_types<-merge(x=castAction, y=merged, by ="northstar_id", all=TRUE)


###########################################################################
###########################################################################

######################### DESCRIPTIVE ANALYSES ############################

###########################################################################
###########################################################################

####BASIC FREQUENCIES#####

# NPS Freq counts and %
prop_nps<-merged %>% group_by(nps_cat) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#NPS averages
ddply(merged, .(survey), summarize, nps_sms_mean=mean(nps))


#DS value Frequencies
prop_ds_value<-merged %>% group_by(ds_value) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Satisfaction - Causes
prop_causes<-merged %>% group_by(causes) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Satisfaction - Communication
prop_communication<-merged %>% group_by(communication) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Satisfaction - Website
prop_website<-merged %>% group_by(website) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Satisfaction - Impact
prop_impact<-merged %>% group_by(impact) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Satisfaction - Community
prop_community<-merged %>% group_by(community) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Satisfaction - Scholarships
prop_scholarships<-merged %>% group_by(scholarships) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#No. Signups Frequencies
prop_signups<-merged %>% group_by(signups_cat) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Years a Member Frequencies
prop_years_member<-merged %>% group_by(years_a_member) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

##########################
#######CROSS TABS#########
##########################

#NPS category x survey segment
CrossTable(merged$survey, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS x survey segment
CrossTable(merged$nps, merged$survey, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#DS value x survey segment
CrossTable(merged$ds_value, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

####################################
##### Satisfaction x Survey ########
####################################

#Average satisfaction scores by survey
ddply(merged, .(survey), summarize, causes_mean=mean(causes), communication_mean=mean(communication),
      website_mean=mean(website), impact_mean=mean(impact), community_mean=mean(community),
                        scholarships_mean=mean(scholarships))
#Causes x survey
CrossTable(merged$causes, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Community x survey
CrossTable(merged$community, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged$communication, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged$website, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged$scholarships, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged$impact, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged$impactful, merged$nps_cat, prop.c=TRUE, prop.r=TRUE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


######################################
########## BAD NICHE ################
######################################

#NPS score for Bad Niche vs. All others - Crosstab for NPS category x Bad Niche
CrossTable(merged$nps_cat, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Excluding Bad Niche#
#Remove Bad Niche
No_badniche<-merged%>%
  dplyr::filter(bad_niche != 1)

#Bad Niche
CrossTable(No_badniche$nps_cat, No_badniche$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$nps, No_badniche$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$nps_cat, No_badniche$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(No_badniche$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$nps, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$ds_value, prop.c=FALSE, prop.r=FALSE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))



CrossTable(merged$rbs, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#Satisfaction
CrossTable(No_badniche$causes, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$communication, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$website, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$scholarships, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$impact, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(No_badniche$community, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Words
CrossTable(No_badniche$nps_cat, No_badniche$inspiring, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS category x survey segment
CrossTable(merged$survey, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS x survey segment
CrossTable(merged$nps, merged$survey, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#DS value x survey segment
CrossTable(merged$ds_value, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#######################################
######### REGRESSION MODELS ##########
#######################################

##Regression models predicting NPS (1-10)

#Function for
plotMod <- function(field, data) {
  require(scales)
  surveyMod.form = as.formula(paste0('nps ~ ',field,' + survey + ',field,':survey'))
  surveyMod <- lm(surveyMod.form, data)

  fieldMod.form = as.formula(paste0('nps ~ ',field))
  fieldMod <- lm(fieldMod.form, data)

  preds <-
    as.tibble(expand.grid(survey = c('Nonniche','sms','niche','full'),
                          predictor = seq(0,5,1))) %>%
    arrange(survey, predictor) %>%
    setNames(c('survey',field))

  preds <- data.table(preds)

  preds[survey!='full',predictions := predict(surveyMod, preds[survey!='full'], type='response')]
  preds[survey=='full',predictions := predict(fieldMod, preds[survey=='full'], type='response')]

  ggplot(preds, aes(x=get(field), y=predictions, color=survey)) +
    geom_line() +
    scale_y_continuous(breaks=pretty_breaks(10)) +
    labs(y="Expected NPS", x=paste(field), title=paste0("Predicted NPS for ",field))

}

cause_action_types %>%
  dplyr::select(contains('_type_')) %>%
  colnames() -> fields

for (i in 1:length(fields)) {
  p <- plotMod(fields[i], cause_action_types)
  print(p)
}


#
# #Donate Something as predictor
# donate_regression_interaction = as.formula('nps ~ action_type_DonateSomething + survey + action_type_DonateSomething:survey')
# model_donate <- lm(donate_regression_interaction, cause_action_types)
# tidy(model_donate)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_DonateSomething = seq(0,10,1))) %>%
#   arrange(survey, action_type_DonateSomething)
#
# preds$predictions <- predict(model_donate, preds, type='response')
# ggplot(preds, aes(x=action_type_DonateSomething, y=predictions, color=survey)) + geom_line()
#
# #Share Something
# share_regression_interaction = as.formula('nps ~ action_type_ShareSomething + survey + action_type_ShareSomething:survey')
# model_share <- lm(share_regression_interaction, cause_action_types)
# tidy(model_share)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_ShareSomething = seq(0,10,1))) %>%
#   arrange(survey, action_type_ShareSomething)
#
# preds$predictions <- predict(model_share, preds, type='response')
# ggplot(preds, aes(x=action_type_ShareSomething, y=predictions, color=survey)) + geom_line()
#
#
# # Face to Face
# face_regression_interaction = as.formula('nps ~ action_type_FacetoFace + survey + action_type_FacetoFace:survey')
# model_face <- lm(face_regression_interaction, cause_action_types)
# tidy(model_face)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_FacetoFace = seq(0,10,1))) %>%
#   arrange(survey, action_type_FacetoFace)
#
# preds$predictions <- predict(model_face, preds, type='response')
# ggplot(preds, aes(x=action_type_FacetoFace, y=predictions, color=survey)) + geom_line()
#
# # Host an Event
# host_regression_interaction = as.formula('nps ~ action_type_HostAnEvent + survey + action_type_HostAnEvent:survey')
# model_hostevent <- lm(host_regression_interaction, cause_action_types)
# tidy(model_hostevent)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_HostAnEvent = seq(0,10,1))) %>%
#   arrange(survey, action_type_HostAnEvent)
#
# preds$predictions <- predict(model_hostevent, preds, type='response')
# ggplot(preds, aes(x=action_type_HostAnEvent, y=predictions, color=survey)) + geom_line()
#
# # Improve a Space
# space_regression_interaction = as.formula('nps ~ action_type_ImproveaSpace + survey + action_type_ImproveaSpace:survey')
# model_space <- lm(space_regression_interaction, cause_action_types)
# tidy(model_space)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_ImproveaSpace = seq(0,10,1))) %>%
#   arrange(survey, action_type_ImproveaSpace)
#
# preds$predictions <- predict(model_space, preds, type='response')
# ggplot(preds, aes(x=action_type_ImproveaSpace, y=predictions, color=survey)) + geom_line()
#
# # Make Something
# make_regression_interaction = as.formula('nps ~ action_type_MakeSomething + survey + action_type_MakeSomething:survey')
# model_make <- lm(make_regression_interaction, cause_action_types)
# tidy(model_make)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_MakeSomething = seq(0,10,1))) %>%
#   arrange(survey, action_type_MakeSomething)
#
# preds$predictions <- predict(model_make, preds, type='response')
# ggplot(preds, aes(x=action_type_MakeSomething, y=predictions, color=survey)) + geom_line()
#
# # Start Something
# start_regression_interaction = as.formula('nps ~ action_type_StartSomething + survey + action_type_StartSomething:survey')
# model_start <- lm(start_regression_interaction, cause_action_types)
# tidy(model_start)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_StartSomething = seq(0,10,1))) %>%
#   arrange(survey, action_type_StartSomething)
#
# preds$predictions <- predict(model_start, preds, type='response')
# ggplot(preds, aes(x=action_type_StartSomething, y=predictions, color=survey)) + geom_line()
#
# # Take a Stand
# stand_regression_interaction = as.formula('nps ~ action_type_TakeaStand + survey + action_type_TakeaStand:survey')
# model_stand <- lm(stand_regression_interaction, cause_action_types)
# tidy(model_stand)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_TakeaStand = seq(0,10,1))) %>%
#   arrange(survey, action_type_TakeaStand)
#
# preds$predictions <- predict(model_stand, preds, type='response')
# ggplot(preds, aes(x=action_type_TakeaStand, y=predictions, color=survey)) + geom_line()
#
# #####CAUSES####
#
# # Animals
# animals_regression_interaction = as.formula('nps ~ cause_type_Animals + survey + cause_type_Animals:survey')
# model_animals <- lm(animals_regression_interaction, cause_action_types)
# tidy(model_animals)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         cause_type_Animals = seq(0,10,1))) %>%
#   arrange(survey, cause_type_Animals)
#
# preds$predictions <- predict(model_animals, preds, type='response')
# ggplot(preds, aes(x=cause_type_Animals, y=predictions, color=survey)) + geom_line()
#
# # Bullying
# bully_regression_interaction = as.formula('nps ~ cause_type_Bullying + survey + cause_type_Bullying:survey')
# model_bully <- lm(bully_regression_interaction, cause_action_types)
# tidy(model_bully)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         cause_type_Bullying = seq(0,10,1))) %>%
#   arrange(survey, cause_type_Bullying)
#
# preds$predictions <- predict(model_bully, preds, type='response')
# ggplot(preds, aes(x=cause_type_Bullying, y=predictions, color=survey)) + geom_line()
#
# # Disasters
# disaster_regression_interaction = as.formula('nps ~ cause_type_Disasters + survey + cause_type_Disasters:survey')
# model_disaster <- lm(disaster_regression_interaction, cause_action_types)
# tidy(model_disaster)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         cause_type_Disasters = seq(0,10,1))) %>%
#   arrange(survey, cause_type_Disasters)
#
# preds$predictions <- predict(model_disaster, preds, type='response')
# ggplot(preds, aes(x=cause_type_Disasters, y=predictions, color=survey)) + geom_line()
#
#
#
#
# # Education
# education_regression_interaction = as.formula('nps ~ cause_type_Education + survey + cause_type_Education:survey')
# model_education <- lm(education_regression_interaction, cause_action_types)
# tidy(model_education)
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         cause_type_Education = seq(0,10,1))) %>%
#   arrange(survey, cause_type_Education)
#
# preds$predictions <- predict(model_education, preds, type='response')
# ggplot(preds, aes(x=cause_type_Education, y=predictions, color=survey)) + geom_line()
#
#
# write.csv(cause_action_types, file = "Q4 causes and actions.csv")
# save(model,file = '~/Desktop/model.rds')
#
#
# plotMod('action_type_DonateSomething',cause_action_types)



causes_actions<-cause_action_types%>%
  dplyr::select(northstar_id,
                action_type_DonateSomething,
                action_type_FacetoFace,
                action_type_HostAnEvent,
                action_type_ImproveaSpace,
                action_type_MakeSomething,
                action_type_NA,
                action_type_ShareSomething,
                action_type_StartSomething,
                action_type_TakeaStand,
                cause_type_Animals,
                cause_type_Bullying,
                cause_type_Disasters,
                cause_type_Discrimination,
                cause_type_Education,
                cause_type_Environment,
                cause_type_Homelessness,
                cause_type_MentalHealth,
                cause_type_NA,
                cause_type_PhysicalHealth,
                cause_type_Poverty,
                cause_type_Relationships,
                cause_type_Sex,
                cause_type_Violence,
                nps,
                nps_cat)%>%
        mutate(
              donate=ifelse(action_type_DonateSomething>0,1,0),
              face=ifelse(action_type_FacetoFace>0,1,0),
              host=ifelse(action_type_HostAnEvent>0,1,0),
              space=ifelse(action_type_ImproveaSpace>0,1,0),
              make=ifelse(action_type_MakeSomething>0,1,0),
              action_NA=ifelse(action_type_NA>0,1,0),
              share=ifelse(action_type_ShareSomething>0,1,0),
              start=ifelse(action_type_StartSomething>0,1,0),
              takestand=ifelse(action_type_TakeaStand>0,1,0),
              animals=ifelse(cause_type_Animals>0,1,0),
              bullying=ifelse(cause_type_Bullying>0,1,0),
              disasters=ifelse(cause_type_Disasters>0,1,0),
              discrimination=ifelse(cause_type_Discrimination>0,1,0),
              education=ifelse(cause_type_Education>0,1,0),
              environment=ifelse(cause_type_Environment>0,1,0),
              homeless=ifelse(cause_type_Homelessness>0,1,0),
              mentalhealth=ifelse(cause_type_MentalHealth>0,1,0),
              cause_NA=ifelse(cause_type_NA>0,1,0),
              health=ifelse(cause_type_PhysicalHealth>0,1,0),
              poverty=ifelse(cause_type_Poverty>0,1,0),
              relationships=ifelse(cause_type_Relationships>0,1,0),
              sex=ifelse(cause_type_Sex>0,1,0),
              violence=ifelse(cause_type_Violence>0,1,0)
                )

############################################
####### LOGISTIC REGRESSION MODELS #########
############################################

#Logistic regression model - Outcome = being a promoter, predictors are satisfaction on touchpoints
logreg_promoter <-glm(promoter~communication_rec + causes_rec + website_rec + scholarships_rec + community_rec +impact_rec,
                      data=logisitic, family=binomial(link="logit"))

summary(logreg_promoter)
exp(coef(logreg_promoter))

logreg_promoter <-glm(promoter~impact_rec,data=logisitic, family=binomial(link="logit"))
summary(logreg_promoter)
exp(coef(logreg_promoter))

# Satisfaction
regression_causes = as.formula('nps ~ causes')
satisfaction_causes <- lm(regression_causes, cause_action_types)
tidy(satisfaction_causes)

# #Actions
# CrossTable(causes_actions$nps_cat, causes_actions$donate, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$face, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$takestand, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$host, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$start, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$make, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$share, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$space, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

# #Causes
# CrossTable(causes_actions$nps_cat, causes_actions$animals, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$bullying, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$disasters, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$discrimination, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$poverty, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$homeless, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$mentalhealth, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$health, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$environment, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$education, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$violence, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$relationships, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
# CrossTable(causes_actions$nps_cat, causes_actions$sex, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#
#
#
# #
# # write.csv(qres_action, file = "Q4 action types.csv")
#
# #merge with NPS
# # merge(x=all, y=dat, by ="northstar_id", all=TRUE)
# #
# # nps_only<-merged%>%
# #   select(northstar_id,nps,nps_cat)
# #
# # #Query for Sohaib to recalibrate Q3 NPS scores- need NSID, signup created at date
# # q_recalib<- paste0("
# #           SELECT ca.northstar_id,
# #            ca.signup_created_at
# #            FROM quasar.users u
#            LEFT JOIN quasar.campaign_activity ca
#            ON u.northstar_id=ca.northstar_id
#            WHERE u.northstar_id IN", a
#           )
#
# qres_recalib <- runQuery(q_recalib, which = 'mysql')
#
# #Get earliest signup date
# minDate <- min(as.Date(substr(qres_recalib$signup_created_at, 1, 10)))
#
# dat <-
#   qres_recalib %>%
#   group_by(northstar_id) %>%
#   summarise(
#     avg_signup_date = as.Date(mean(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01')
#   )


#Merge db data with survey data
# Merged<- all %>%
#   left_join(qres, by = 'northstar_id')
# merged_recalib<-merge(x=all, y=dat, by ="northstar_id", all=TRUE)
#
#
# vars<-c("northstar_id","nps","survey","avg_signup_date")
# Q4_avgsignup<-merged_recalib[vars]
#
# write.csv(Q4_avgsignup, file = "Q4 NPS for recalibration.csv")

#Merge db recalibration data (with signup dates) with NPS survey data


#Frequencies
# attach(merged)
# value<- table(ds_value,count(ds_value))
# margin.table(merged,ds_value)
# prop.table(merged,ds_value)


#create binary variables
logisitic<-merged%>%
  mutate(
    community_rec=(ifelse(community==5,1,0)),
    causes_rec=(ifelse(causes==5,1,0)),
    website_rec=(ifelse(website==5,1,0)),
    communication_rec=(ifelse(communication==5,1,0)),
    scholarships_rec=(ifelse(scholarships==5,1,0)),
    impact_rec=ifelse(impact==5,1,0),
    promoter=ifelse(nps_cat=='Promoter',1,0)
  )


# nps_by_survey=table(merged$
# nps_by_survey=table(merged$nps_cat,merged$survey)ny
# colnames(nps_by_survey) <- c("Detractor", "Persuadable", "Promoter", "Total_N", "Detract_Pct", "Persuadable_Pct", "Promoter_Pct")
# (nps_by_survey_frq_pct <- cbind(tbl, rowSums(tbl), (tbl) / rowSums(tbl)))
# (nps_by_survey_pct <- cbind(rowSums(tbl), (tbl) / rowSums(tbl)))
# #Chi-square test
# chisq.test(nps_by_survey)

#Alternative way to do crosstab and chi-sq
# attach(merged)
# mytable<-xtabs(~nps_cat+survey, data=merged)
# ftable(mytable)
# summary(mytable)

##GLADITOR - COMPS###
gladiator<-read.csv('~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Q4 identify Sentiment respondents in Gladiator.csv')
  # ##Merge with db data
# gladiator_copy<-gladiator
# b<-prepQueryObjects(gladiator$user_id)
#
# #Query to pull db data and then link to members who completed NPS survey
# q_comp<- paste0("
#            SELECT DISTINCT
# user_id
#            FROM gladiator.users
#            WHERE user_id IN", b,
#            "GROUP BY user_id")
#
# qres_comp <- runQuery(q_comp, which = 'mysql')

merged_all<-merge(x=cause_action_types, y=gladiator, by ="northstar_id", all=TRUE)

CrossTable(merged_all$comp, merged_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#Signups
CrossTable(merged$signups_cat, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Rbs
CrossTable(merged$rbs_cat, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$community, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS without bad-Niche
CrossTable(merged$survey, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$bad_niche, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#Bad Niche
CrossTable(merged_all$nps, merged_all$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(merged$bad_niche, merged$nps, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$reengage, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$reengage, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))





#DS value
CrossTable(merged$ds_value, merged$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$community, merged$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$impact, merged$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$nps,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


write.csv(No_badniche, file = 'Bad Niche.csv')
write.csv(merged, file = "Merged.csv")



#Tornado chart
# ggplot(nichePivot, aes(x=campaign, y=nps, fill=Barrier)) +
#   geom_bar(stat='identity', position='dodge') + coord_flip() +
#   facet_wrap(~variable)


# Nicheactivated<-qres%>%
#   dplyr::count(northstar_id, Niche_activated)

# cause_action_types%>%
#   summarise(
#     Count = sum(action_type_ShareSomething)
#   ) %>%
#   mutate(Proportion.donate = Count / sum(Count))

#Alternative code for Basic Freq counts including missing
# table(all$nps_cat, exclude=NULL)
# table(all$nps_cat)
# all %>% group_by(nps_cat) %>%
#   summarise(count = n())

#Plot average signup dates
#Paramaters to be inserted into query
# yearAgo <- Sys.Date() - 365
# ggplot(dat, aes(x=avg_signup_date)) +
#   geom_density() + ggtitle('Average signup date') +
#   scale_x_date(breaks=pretty_breaks(15))

#Check there aren't duplicates (members who answered survey more than once)
duplicate<-merged%>%
  filter(!duplicated(northstar_id))

#Set survey weights so they match DS membership reg source %
merged_Q4_all<-all %>%
  mutate(
    weight=
      case_when(
        survey=='niche' ~ 0.99473583,
        survey=='sms' ~ 0.82866182,
        survey=='Nonniche' ~ 1.239375)
  )

#set weights
merged_Q4_all.w<-svydesign(id = ~1, data = all, weights = merged_Q4_all$weight)

#Weighted Reg Source
prop.table(svytable(~survey, design=merged_Q4_all.w))
#Unweighted Reg Source (original survey respondents)
prop.table(table(merged_Q4_all$survey))

#unweighted NPS
prop.table(table(merged_Q4_all$nps_cat))
#weighted NPS
prop.table(svytable(~nps_cat, design=merged_Q4_all.w))
