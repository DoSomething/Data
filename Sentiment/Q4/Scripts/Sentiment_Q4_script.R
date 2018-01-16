library(tidyverse)
library(data.table)
library(broom)
library(gmodels)
library(MASS)

source('config/init.R')
source('config/mySQLConfig.R')

#Import Type form data
Niche_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Niche 12.1.2017.csv")
Niche_typeform$survey<-"niche"
Niche_typeform$age<-"n/a"
NonNiche_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Non Niche 12.01.2017.csv")
NonNiche_typeform$survey<-"Nonniche"
NonNiche_typeform$age<-"n/a"
SMS_typeform<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/SMS only 12.1.2017.csv")
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

# desiredColnames <- c('id_typeform', 'nps', 'nps_reason',...)
#
# for (i in 1:length(colnames(SMS_typeform))) {
#   colnames(SMS_typeform)[i] <- desiredColnames[i]
# }
#
# SMS_typeform %>%
#   setNames(desiredColnames)

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
# setDT(all)[nps<7, nps_cat:= "Detractor"]
# all[nps==7|nps==8, nps_cat:= "Persuadable"]
# all[nps>8, nps_cat:= "Promoter"]
all<-all %>%
  mutate(
    nps_cat =
      case_when(nps<7 ~ 'Detractor',
                nps %in% c(7,8) ~ 'Persuadable',
                nps>8 ~ 'Promoter')
  )

# write.csv(all, file = "all.csv")

##Merge with db data
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

Nicheactivated<-qres%>%
  dplyr::count(northstar_id, Niche_activated)

merged<-merge(x=all, y=qres, by ="northstar_id", all=TRUE)

merged<-merged%>%
  # filter(survey=='niche')%>%
  # dplyr::select(created_at, last_accessed, last_logged_in, total_signups, Niche_activated, Niche_activated_other, source, nps_cat, total_rbs)%>%
  mutate(
    bad_niche=ifelse(survey =='niche' & Niche_activated==0 & total_rbs==0 & total_signups<=1,1,0)
  )

#crosstab of bad niche x NPS
CrossTable(merged$nps_cat, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$survey, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

write.csv(merged, file = "Bad Niche.csv")

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

#Query for action types - already setting who are in the sample but don't want distinct
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

#reshape from short to long form
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

cause_action_types%>%
  summarise(
    Count = sum(action_type_ShareSomething)
  ) %>%
  mutate(Proportion.donate = Count / sum(Count))

#Donate Something
donate_regression_interaction = as.formula('nps ~ action_type_DonateSomething + survey + action_type_DonateSomething:survey')
model_donate <- lm(donate_regression_interaction, cause_action_types)
tidy(model_donate)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_DonateSomething = seq(0,10,1))) %>%
  arrange(survey, action_type_DonateSomething)

preds$predictions <- predict(model_donate, preds, type='response')
ggplot(preds, aes(x=action_type_DonateSomething, y=predictions, color=survey)) + geom_line()

#Share Something
share_regression_interaction = as.formula('nps ~ action_type_ShareSomething + survey + action_type_ShareSomething:survey')
model_share <- lm(share_regression_interaction, cause_action_types)
tidy(model_share)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_ShareSomething = seq(0,10,1))) %>%
  arrange(survey, action_type_ShareSomething)

preds$predictions <- predict(model_share, preds, type='response')
ggplot(preds, aes(x=action_type_ShareSomething, y=predictions, color=survey)) + geom_line()


# Face to Face
face_regression_interaction = as.formula('nps ~ action_type_FacetoFace + survey + action_type_FacetoFace:survey')
model_face <- lm(face_regression_interaction, cause_action_types)
tidy(model_face)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_FacetoFace = seq(0,10,1))) %>%
  arrange(survey, action_type_FacetoFace)

preds$predictions <- predict(model_face, preds, type='response')
ggplot(preds, aes(x=action_type_FacetoFace, y=predictions, color=survey)) + geom_line()

# Host an Event
host_regression_interaction = as.formula('nps ~ action_type_HostAnEvent + survey + action_type_HostAnEvent:survey')
model_hostevent <- lm(host_regression_interaction, cause_action_types)
tidy(model_hostevent)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_HostAnEvent = seq(0,10,1))) %>%
  arrange(survey, action_type_HostAnEvent)

preds$predictions <- predict(model_hostevent, preds, type='response')
ggplot(preds, aes(x=action_type_HostAnEvent, y=predictions, color=survey)) + geom_line()

# Improve a Space
space_regression_interaction = as.formula('nps ~ action_type_ImproveaSpace + survey + action_type_ImproveaSpace:survey')
model_space <- lm(space_regression_interaction, cause_action_types)
tidy(model_space)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_ImproveaSpace = seq(0,10,1))) %>%
  arrange(survey, action_type_ImproveaSpace)

preds$predictions <- predict(model_space, preds, type='response')
ggplot(preds, aes(x=action_type_ImproveaSpace, y=predictions, color=survey)) + geom_line()

# Make Something
make_regression_interaction = as.formula('nps ~ action_type_MakeSomething + survey + action_type_MakeSomething:survey')
model_make <- lm(make_regression_interaction, cause_action_types)
tidy(model_make)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_MakeSomething = seq(0,10,1))) %>%
  arrange(survey, action_type_MakeSomething)

preds$predictions <- predict(model_make, preds, type='response')
ggplot(preds, aes(x=action_type_MakeSomething, y=predictions, color=survey)) + geom_line()

# Start Something
start_regression_interaction = as.formula('nps ~ action_type_StartSomething + survey + action_type_StartSomething:survey')
model_start <- lm(start_regression_interaction, cause_action_types)
tidy(model_start)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_StartSomething = seq(0,10,1))) %>%
  arrange(survey, action_type_StartSomething)

preds$predictions <- predict(model_start, preds, type='response')
ggplot(preds, aes(x=action_type_StartSomething, y=predictions, color=survey)) + geom_line()

# Take a Stand
stand_regression_interaction = as.formula('nps ~ action_type_TakeaStand + survey + action_type_TakeaStand:survey')
model_stand <- lm(stand_regression_interaction, cause_action_types)
tidy(model_stand)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        action_type_TakeaStand = seq(0,10,1))) %>%
  arrange(survey, action_type_TakeaStand)

preds$predictions <- predict(model_stand, preds, type='response')
ggplot(preds, aes(x=action_type_TakeaStand, y=predictions, color=survey)) + geom_line()

#####CAUSES####

# Animals
animals_regression_interaction = as.formula('nps ~ cause_type_Animals + survey + cause_type_Animals:survey')
model_animals <- lm(animals_regression_interaction, cause_action_types)
tidy(model_animals)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        cause_type_Animals = seq(0,10,1))) %>%
  arrange(survey, cause_type_Animals)

preds$predictions <- predict(model_animals, preds, type='response')
ggplot(preds, aes(x=cause_type_Animals, y=predictions, color=survey)) + geom_line()

# Bullying
bully_regression_interaction = as.formula('nps ~ cause_type_Bullying + survey + cause_type_Bullying:survey')
model_bully <- lm(bully_regression_interaction, cause_action_types)
tidy(model_bully)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        cause_type_Bullying = seq(0,10,1))) %>%
  arrange(survey, cause_type_Bullying)

preds$predictions <- predict(model_bully, preds, type='response')
ggplot(preds, aes(x=cause_type_Bullying, y=predictions, color=survey)) + geom_line()

# Disasters
disaster_regression_interaction = as.formula('nps ~ cause_type_Disasters + survey + cause_type_Disasters:survey')
model_disaster <- lm(disaster_regression_interaction, cause_action_types)
tidy(model_disaster)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        cause_type_Disasters = seq(0,10,1))) %>%
  arrange(survey, cause_type_Disasters)

preds$predictions <- predict(model_disaster, preds, type='response')
ggplot(preds, aes(x=cause_type_Disasters, y=predictions, color=survey)) + geom_line()




# Education
education_regression_interaction = as.formula('nps ~ cause_type_Education + survey + cause_type_Education:survey')
model_education <- lm(education_regression_interaction, cause_action_types)
tidy(model_education)

preds <-
  as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
                        cause_type_Education = seq(0,10,1))) %>%
  arrange(survey, cause_type_Education)

preds$predictions <- predict(model_education, preds, type='response')
ggplot(preds, aes(x=cause_type_Education, y=predictions, color=survey)) + geom_line()


write.csv(cause_action_types, file = "Q4 causes and actions.csv")
save(model,file = '~/Desktop/model.rds')


plotMod('action_type_DonateSomething',cause_action_types)

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
#Actions
CrossTable(causes_actions$nps_cat, causes_actions$donate, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$face, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$takestand, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$host, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$start, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$make, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$share, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$space, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Causes
CrossTable(causes_actions$nps_cat, causes_actions$animals, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$bullying, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$disasters, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$discrimination, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$poverty, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$homeless, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$mentalhealth, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$health, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$environment, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$education, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$violence, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$relationships, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(causes_actions$nps_cat, causes_actions$sex, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))



#
# write.csv(qres_action, file = "Q4 action types.csv")

#merge with NPS
# merge(x=all, y=dat, by ="northstar_id", all=TRUE)
#
# nps_only<-merged%>%
#   select(northstar_id,nps,nps_cat)
#
# #Query for Sohaib to recalibrate Q3 NPS scores- need NSID, signup created at date
# q_recalib<- paste0("
#           SELECT ca.northstar_id,
#            ca.signup_created_at
#            FROM quasar.users u
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

#Crosstabs of NPS category by segment
CrossTable(merged$survey, merged$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$ds_value, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Causes by survey
CrossTable(merged$causes, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Community by survey
CrossTable(merged$community, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication by survey
CrossTable(merged$communication, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication by survey
CrossTable(merged$website, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication by survey
CrossTable(merged$scholarships, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication by survey
CrossTable(merged$impact, merged$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication by survey
CrossTable(merged$impactful, merged$nps_cat, prop.c=TRUE, prop.r=TRUE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Plot average signup dates

#Paramaters to be inserted into query
yearAgo <- Sys.Date() - 365
ggplot(dat, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Average signup date') +
  scale_x_date(breaks=pretty_breaks(15))

#Basic Frequencies
# table(all$nps_cat)
all %>% group_by(nps_cat) %>%
  summarise(count = n())

#Basic Freqs including missing
table(all$nps_cat, exclude=NULL)

#Frequencies for NPS category
prop_nps<-merged %>% group_by(nps_cat) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Frequencies for DS value
prop_ds_value<-merged %>% group_by(ds_value) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Frequencies for DS value
prop_caring<-merged %>% group_by(caring) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Frequencies for Signups
prop_signups<-merged %>% group_by(signups_cat) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

#Years a Member
prop_years_member<-merged %>% group_by(years_a_member) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

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


#DS value
CrossTable(merged$ds_value, merged$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$community, merged$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$impact, merged$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$nps,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


write.csv(No_badniche, file = 'Bad Niche.csv')
write.csv(merged, file = "Merged.csv")


CrossTable(merged$causes, merged$survey, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$communication, merged$survey,  prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$website, merged$survey, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$scholarships, merged$survey, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$impact, merged$survey, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged$community, merged$survey, prop.c=TRUE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Tornado chart
# ggplot(nichePivot, aes(x=campaign, y=nps, fill=Barrier)) +
#   geom_bar(stat='identity', position='dodge') + coord_flip() +
#   facet_wrap(~variable)