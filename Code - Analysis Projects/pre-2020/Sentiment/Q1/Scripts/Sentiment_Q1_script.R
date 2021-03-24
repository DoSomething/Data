library(tidyverse)
library(data.table)
library(broom)
library(gmodels)
library(MASS)
library(plyr)
library(survey)
library(data.table)
library(ggpubr)


source('config/init.R')
source('config/mySQLConfig.R')
source('Q1/Scripts/getSampledNPS.R')

###########################################################################
###########################################################################

###################################
#########DATA PREPPING#############

###########################################################################
###########################################################################

#Import Type form data#
Niche_typeform_Q1<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q1 2018/Data/03_06_2018/Niche_03_06_2018.csv")
Niche_typeform_Q1$survey<-"niche"
Niche_typeform_Q1$age<-"n/a"
NonNiche_typeform_Q1<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q1 2018/Data/03_06_2018/Non_Niche_03_06_2018.csv")
NonNiche_typeform_Q1$survey<-"Nonniche"
NonNiche_typeform_Q1$age<-"n/a"
SMS_typeform_Q1<-read_csv("~/Documents/Sentiment Survey/Member Sentiment Q1 2018/Data/03_06_2018/SMS_03_06_2018.csv")
SMS_typeform_Q1$survey<-"sms"

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
                     'community',
                     'impact',
                     'impact_3reason',
                     'impact_4reason',
                     'impact_5reason',
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
                         'community',
                         'impact',
                         'impact_3reason',
                         'impact_4reason',
                         'impact_5reason',
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
for (i in 1:length(colnames(Niche_typeform_Q1))) {
  colnames(Niche_typeform_Q1)[i] <- desiredColnames[i]
}

for (i in 1:length(colnames(NonNiche_typeform_Q1))) {
  colnames(NonNiche_typeform_Q1)[i] <- desiredColnames[i]
}

Niche_typeform_Q1 %>%
  setNames(desiredColnames)

NonNiche_typeform_Q1%>%
  setNames(desiredColnames)

#Set names for SMS survey
for (i in 1:length(colnames(SMS_typeform_Q1))) {
  colnames(SMS_typeform_Q1)[i] <- desiredColnames_sms[i]
}

SMS_typeform_Q1 %>%
  setNames(desiredColnames_sms)


#Merge all datasets
all_Q1<-rbind(Niche_typeform_Q1,NonNiche_typeform_Q1,SMS_typeform_Q1)

#create NPS categories
all_Q1<-all_Q1 %>%
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

write.csv(all_Q1, file = "all Q1.csv")


###########################################################################
###########################################################################

##########LINKING SENTIMENT SURVEY WITH NSID DATABASE DATA#################

###########################################################################
###########################################################################


##Concatenate NSIDs so you can include them in SQL query as object 'a'
all_copy<-all_Q1
a<-prepQueryObjects(all_copy$northstar_id)

#Query to pull db data and then link to members who completed NPS survey. Remove Niche activated definition from Q4. Using engaged for Q1.
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
            count(DISTINCT ul.last_accessed) as 'site_visits'
          FROM quasar.users u
          LEFT JOIN quasar.campaign_activity ca
          ON u.northstar_id=ca.northstar_id
          LEFT JOIN quasar.users_log ul
          ON u.northstar_id=ul.northstar_id
          WHERE u.northstar_id IN", a,
          "GROUP BY u.northstar_id")

qres_Q1 <- runQuery(q, which = 'mysql')

#Create variables for signup and rb count categories
qres_Q1<-qres_Q1 %>%
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
merged_Q1<-merge(x=all_Q1, y=qres_Q1, by ="northstar_id", all=TRUE)

write.csv(merged_Q1_all, file = "all Q1 merged.csv")

#Check code for 'Bad Niche' are only in Niche
# CrossTable(merged$survey, merged$bad_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#
# write.csv(merged, file = "Bad Niche.csv")
#Identify "Engaged" Niche users.
q_niche<- paste0("
                 SELECT
                 distinct a.northstar_id
                 , a.created_at
                 , a.event_name
                 , a.timestamp
                 FROM (
                 -- web activation
                 SELECT
                 u.northstar_id AS 'northstar_id'
                 , u.created_at AS 'created_at'
                 , 'activated' AS 'event_name'
                 , l.last_logged_in AS 'timestamp'
                 FROM
                 quasar.users u
                 LEFT JOIN
                 quasar.users_log l
                 ON
                 l.northstar_id = u.northstar_id
                 WHERE
                 u.source = 'niche'
                 AND l.last_logged_in <> '0000-00-00 00:00:00'
                 AND l.last_logged_in <> '1970-01-01 00:00:00'
                 GROUP BY
                 u.northstar_id
                 HAVING
                 min(l.last_logged_in) > u.created_at
                 UNION ALL
                 -- sms signups
                 SELECT
                 u.northstar_id AS 'northstar_id'
                 , u.created_at AS 'created_at'
                 , 'sms_signup' AS 'event_name'
                 , MAX(signup_created_at) AS 'timestamp'
                 FROM
                 quasar.users u
                 LEFT JOIN
                 quasar.campaign_activity c
                 ON
                 c.northstar_id = u.northstar_id
                 LEFT JOIN
                 quasar.users_log l
                 ON
                 l.northstar_id = u.northstar_id
                 WHERE
                 u.source = 'niche'
                 GROUP BY
                 u.northstar_id
                 HAVING
                 count(distinct c.signup_id) > 1
                 AND min(l.last_logged_in) <= u.created_at
                 ) AS a
                 WHERE a.northstar_id IN", a,
                 "GROUP BY
                 a.northstar_id")

qres_Q1_engagedniche <- runQuery(q_niche, which = 'mysql')

#merge Engaged Niche data with data.
#Merge with NPS Sentiment survey
merged_Q1_all<-merge(x=merged_Q1, y=qres_Q1_engagedniche, by ="northstar_id", all=TRUE)

#Create engaged Niche group and Set survey weights so they match DS membership reg source %
merged_Q1_all<-merged_Q1_all %>%
  mutate(
    Niche_engaged = ifelse(event_name=='activated' | event_name=='sms_signup',1,0),
    Niche_unengaged = ifelse(survey=='niche' & is.na(Niche_engaged),1,0),
    weight=
      case_when(
        survey=='niche' ~ 1.19,
        survey=='sms' ~ 1.14,
        survey=='Nonniche' ~ 0.83)
    )

#set weights
merged_Q1_all.w<-svydesign(id = ~1, data = merged_Q1_all, weights = merged_Q1_all$weight)

#Check weighting
#Weighted Reg Source
prop.table(svytable(~survey, design=merged_Q1_all.w))
#Unweighted Reg Source (original survey respondents)
prop.table(table(merged_Q1_all$survey))

#unweighted NPS
prop.table(table(merged_Q1_all$nps_cat))
#weighted NPS
prop.table(svytable(~nps_cat, design=merged_Q1_all.w))

#NPS category for Niche Engaged (n=76)
CrossTable(merged_Q1_all$Niche_engaged, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=TRUE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#NPS distribution for Niche Engaged
CrossTable(merged_Q1_all$nps, merged_Q1_all$Niche_engaged, prop.c=TRUE, prop.r=TRUE, prop.t=TRUE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS category for Niche Unengaged (n=60)
CrossTable(merged_Q1_all$Niche_unengaged, merged_Q1_all$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#NPS distribution for Niche Unengaged (n=60)
CrossTable(merged_Q1_all$nps, merged_Q1_all$Niche_unengaged, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


###########################################################################
###########################################################################

##############LINKING CAMPAIGN DATA TO SENTIMENT DATA #####################

###########################################################################
###########################################################################


#Query for pulling campaigns, causes, and action types for those who completed Sentiment survey
campaigns<- paste0("
        SELECT DISTINCT
          ca.northstar_id,
                      ca.signup_id,
                      ca.signup_created_at,
                      ci.*
                      FROM quasar.campaign_activity ca
                      LEFT JOIN
                      (SELECT
                      i.campaign_run_id,
                      max(i.campaign_node_id_title) as campaign,
                      max(i.campaign_cause_type) as 'cause_type',
                      max(i.campaign_action_type) as'action_type'
                      FROM quasar.campaign_info i
                      GROUP BY i.campaign_run_id) ci
                      ON ca.campaign_run_id = ci.campaign_run_id
                      WHERE ca.northstar_id IN", a)
#
Q1_campaigns <-runQuery(campaigns, which ='mysql') %>%
  mutate(
    campaign = gsub("[^[:alnum:]]", "", campaign)
  )

#Only choose campaigns with 100+ signups
Medium <-
  c('MissinginHistory','ThanksaBillion', 'DoSomethingAboutGunViolence', 'RideSeek',
    'GrabtheMic', 'RideSeek', 'FeedingBetterFutures')
Low <- c('ShowerSongs','MirrorMessages','MyBigRegret','ThumbWars',
        'TreatYoFriends', 'ThumbWars', 'TeamUpDreamUp', 'SaveTheMascots')

topCamp <-
  Q1_campaigns %>%
  group_by(campaign) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  filter(campaign %in% c(Medium, Low))

merged_Q1_all%<>%
  mutate(
    group =
      case_when(survey=='niche' ~ 'niche',
                survey=='sms' ~ 'sms_only',
                survey=='Nonniche' ~ 'other', TRUE ~ ' ')
  )

#Remove Niche members
merged_Q1_all_noniche<-merged_Q1_all%>%
  filter(survey!=0)

campaignList <- unique(topCamp$campaign)
percentDidCampaign <- data.frame()
for (i in 1:length(campaignList)) {
  temp <- percentDid(campaignList[i], Q1_campaigns)
  percentDidCampaign <- rbind(percentDidCampaign, temp)
}

uncastCombine <-
  merged_Q1_all %>%
  left_join(Q1_campaigns)
npsCampaign <- data.frame()
for (i in 1:length(campaignList)) {
  # print(campaignList[i])
  temp <- npsDidCampaign(uncastCombine, campaignList[i], 10)
  npsCampaign <- rbind(npsCampaign, temp)
}
npsCampaign %>% arrange(-nps)
castAction <-
  merged_Q1_all %>%
  left_join(
    Q1_campaigns %>%
      filter(campaign %in% topCamp$campaign) %>%
      mutate(
        campaign = gsub("[^[:alnum:]]", "", campaign)
      ) %>%
      dplyr::select(northstar_id, campaign) %>%
      dcast(northstar_id ~ paste0('campaign_',campaign))
  )%>%
  mutate(
    group =
      case_when(survey=='niche' ~ 'niche',
                survey=='sms' ~ 'sms_only',
                survey=='Nonniche' ~ 'other', TRUE ~ ' ')
  )

nameList <- castAction %>% select(starts_with('campaign')) %>% colnames()
campaignImpact <- data.table()
p <- c()
for (i in 1:length(nameList)) {
  p[i] <- plotMod(nameList[i],castAction)
  campaignImpact <- rbind(campaignImpact, data.table(campaign=nameList[i], impact=p[i]))
}
campaignImpact[,campaign:=gsub('campaign_','',campaign)]

result <-
  as.tibble(campaignImpact) %>%
  left_join(percentDidCampaign) %>%
  left_join(npsCampaign) %>%
  arrange(-nps) %>%
  mutate(
    Barrier = ifelse(campaign %in% Medium, 'Medium',
                     ifelse(campaign %in% Low, 'Low', 'Other'))
  )
result$campaign <- factor(result$campaign, levels = result$campaign[order(result$Barrier,-result$nps)])

ggplot(result, aes(x=campaign, y=nps, fill=Barrier)) +
  geom_bar(stat='identity', position='dodge') +
  coord_flip() + geom_text(aes(label=nps), size=4) +
  theme(text = element_text(size=14))

nichePivot <-
  result %>%
  select(campaign, npsNiche, npsNonNiche, Barrier, countNiche, countNonNiche) %>%
  melt(value.var=c('nps','npsNiche','npsNonNiche','countNiche','countNonNiche'),
       value.name='nps', variable.names='Type')

ggplot(nichePivot, aes(x=campaign, y=nps, fill=Barrier)) +
  geom_bar(stat='identity', position='dodge') + coord_flip() +
  geom_text(aes(label=nps), size=4) +
  facet_wrap(~variable)

nonnichePivot <-
  result %>%
  select(campaign, npsNonNiche, Barrier, countNonNiche) %>%
  melt(value.var=c('nps','npsNonNiche','countNonNiche'),
       value.name='nps', variable.names='Type')

ggplot(nonnichePivot, aes(x=campaign, y=nps, fill=Barrier)) +
  geom_bar(stat='identity', position='dodge') + coord_flip() +
  geom_text(aes(label=nps), size=3) +
  facet_wrap(~variable, scale = 'free_x')

#NPS scores for members who signedup for Low campaigns vs. Medium campaigns (campaigns in top 9)
aggFrame <-
  rbind(
    data.frame(Barrier='Low', nps=npsDidCampaign(uncastCombine, Low)),
    data.frame(Barrier='Medium', nps=npsDidCampaign(uncastCombine, Medium))
  )



campaigns<-Q1_campaigns %>% group_by(Q1_campaigns$campaign) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)
  )

write.csv(campaigns, file = "Q1 campaigns.csv")

#Reshape data from short to long form and count # times action or cause done by each member
# castAction <-
  # qres_action%>%
  # mutate(action_type = gsub(' ', '', action_type)) %>%
  # dplyr::select(northstar_id, action_type) %>%
  # dcast(northstar_id ~ paste0('action_type_',action_type)) %>%
  # left_join(
  #   qres_action%>%
  #     mutate(cause_type = gsub(' ', '', cause_type)) %>%
  #     dplyr::select(northstar_id, cause_type) %>%
  #     dcast(northstar_id ~ paste0('cause_type_',cause_type))
  # )

#Merge action/cause types with Type Form and DB data
cause_action_types_Q1<-merge(x=castAction, y=merged_Q1_all, by ="northstar_id", all=TRUE)



###########################################################################
###########################################################################

##############LINKING MEMBER EVENT LOG TO SENTIMENT DATA ##################

###########################################################################
###########################################################################

q_mel<- paste0("SELECT
                  mel.northstar_id,
                  mel.action_type,
                  mel.source,
                  MAX(mel.timestamp) as 'time',
                  MAX(CASE WHEN DATE(mel.`timestamp`) < '2017-11-15' THEN 1 ELSE 0 END) as 'active_before_q1'
                FROM quasar.member_event_log mel
                 WHERE mel.timestamp < '2018-02-21' AND mel.northstar_id IN", a,
                 "GROUP BY
                 mel.northstar_id")

qres_Q1_mel <- runQuery(q_mel, which = 'mysql')

nps_mel<-merged_Q1_all%>%
  select(northstar_id,survey, Niche_engaged, Niche_unengaged, nps, nps_cat, submit_date, reengage)

mel_nps<-merge(x=qres_Q1_mel, y=nps_mel, by ="northstar_id", all=TRUE)

mel_nps<-mel_nps%>%
  mutate(
  survey_submit = as.Date(paste0(submit_date,'%Y-%m-%d %H:%M:%S')),
  last_active = as.Date(time, '%Y-%m-%d %H:%M:%S'),
  time_to_survey = survey_submit - last_active)

#Select relevant variables
mel_nps<-mel_nps%>% select(northstar_id, survey, survey_submit, last_active, time_to_survey, nps, nps_cat, reengage)
#Look at quantiles for # days before survey
quantile(mel_nps$time_to_survey)
mean(mel_nps$time_to_survey)
#weighted
svymean(~mel_nps$time_to_survey,merged_Q1_all.w)

mel_nps_time<-mel_nps%>%
  mutate(
    time_cat=
      case_when(time_to_survey < 15 ~ 'Last active within 14 days before survey',
                time_to_survey >14 & time_to_survey <50 ~ 'Last active 15-49 days before survey',
                time_to_survey >49 & time_to_survey <127 ~ 'Last active 50-126 days before survey',
                time_to_survey >126 ~ 'Last active 127 days or more before survey'))

# time last active x NPS category
CrossTable(mel_nps_time$time_cat, mel_nps_time$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(mel_nps_time$time_cat, mel_nps_time$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for SMS
mel_nps_sms<-mel_nps_time%>%
  filter(survey=='sms')
CrossTable(mel_nps_sms$time_cat, mel_nps_sms$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for Nonniche
mel_nps_nonniche<-mel_nps_time%>%
  filter(survey=='Nonniche')
CrossTable(mel_nps_nonniche$time_cat, mel_nps_nonniche$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for Niche
mel_nps_niche<-mel_nps_time%>%
  filter(survey=='niche')
CrossTable(mel_nps_niche$time_cat, mel_nps_niche$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for Nonniche + Typical
mel_nps_nosms<-mel_nps_time%>%
  filter(survey=='Nonniche'| survey=='niche')

quantile(mel_nps_nosms$time_to_survey)

mel_nosms<-mel_nps_nosms%>%
  mutate(
    time_cat=
      case_when(time_to_survey < 29 ~ 'Last active within 28 days before (4 wks)',
                time_to_survey >=28 & time_to_survey <=84 ~ 'Last active 28-84 days before (12wks)',
                time_to_survey >=85 & time_to_survey <=168 ~ 'Last active 85-168 days before survey (24 wks)',
                time_to_survey >168 ~ 'Last active 168 days or more before (24+ wks)'))

CrossTable(mel_nosms$time_cat, mel_nosms$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(mel_nosms$time_cat, mel_nosms$reengage, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#NPS for Q4 Nonniche + Typical
mel_nps_nosms_q4<-mel_nps_time_q4%>%
  filter(survey=='Nonniche'| survey=='niche')

quantile(mel_nps_nosms_q4$time_to_survey)

mel_nosms_q4<-mel_nps_nosms_q4%>%
  mutate(
    time_cat=
      case_when(time_to_survey < 29 ~ 'Last active within 28 days before (4 wks)',
                time_to_survey >=28 & time_to_survey <=84 ~ 'Last active 28-84 days before (12wks)',
                time_to_survey >=85 & time_to_survey <=168 ~ 'Last active 85-168 days before survey (24 wks)',
                time_to_survey >168 ~ 'Last active 168 days or more before (24+ wks)'))

CrossTable(mel_nosms_q4$time_cat, mel_nosms_q4$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


###########################################################################
###########################################################################

##############LINKING CAMPGAIGN SIGN UP DATE TO SENTIMENT DATA #############

###########################################################################
###########################################################################
a_signup=prepQueryObjects(merged_Q1_all$northstar_id)
yearAgo <- as.Date('2017-12-05') - 365
q_signup <-
  paste0("SELECT
         c.northstar_id,
         c.signup_id,
        c.signup_source,
         date(c.signup_created_at) AS signup_date
         FROM quasar.campaign_activity c
         WHERE c.signup_created_at < '2018-02-21' AND c.northstar_id IN",a_signup,"
         AND c.signup_created_at >= '",yearAgo,"'"
  )

q_signup <- runQuery(q_signup, 'mysql')

#Select most recent sign up date before survey
lastSignup <-
  q_signup %>%
  mutate(signup_date = as.Date(signup_date)) %>%
  group_by(northstar_id) %>%
  summarise(last_signup = max(signup_date))

nps_mel<-merged_Q1_all%>%
  select(northstar_id,survey, Niche_engaged, Niche_unengaged, nps, nps_cat, submit_date)

signup_nps<-merge(x=lastSignup, y=nps_mel, by ="northstar_id", all=TRUE)

signup_nps<-signup_nps%>%
  mutate(
    survey_submit = as.Date(paste0(submit_date,'%Y-%m-%d %H:%M:%S')),
    last_active = as.Date(last_signup, '%Y-%m-%d %H:%M:%S'),
    time_to_survey = survey_submit - last_active)

#Select relevant variables
signup_nps<-signup_nps%>% select(northstar_id, survey, survey_submit, last_active, time_to_survey, nps, nps_cat)
#Look at quantiles for # days before survey
quantile(signup_nps$time_to_survey)

signup_nps_time<-signup_nps%>%
  mutate(
    time_cat=
      case_when(time_to_survey < 15 ~ 'Last active within 14 days before survey',
                time_to_survey >14 & time_to_survey <51 ~ 'Last active 15-50 days before survey',
                time_to_survey >50 & time_to_survey <129 ~ 'Last active 51-128 days before survey',
                time_to_survey >128 ~ 'Last active 129 days or more before survey'))

CrossTable(signup_nps_time$time_cat, signup_nps_time$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for SMS
signup_nps_sms<-signup_nps_time%>%
  filter(survey=='sms')
CrossTable(signup_nps_sms$time_cat, signup_nps_sms$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for Nonniche
signup_nps_nonniche<-signup_nps_time%>%
  filter(survey=='Nonniche')
CrossTable(signup_nps_nonniche$time_cat, signup_nps_nonniche$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#NPS for Niche
signup_nps_niche<-signup_nps_time%>%
  filter(survey=='niche')
CrossTable(signup_nps_niche$time_cat, signup_nps_niche$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))



###########################################################################
###########################################################################

##############LINKING C.IO EVENT LOG TO SENTIMENT DATA ##################

###########################################################################
###########################################################################

q_cio<- paste0("SELECT ci.northstar_id,
              sum(case when ci.event_id <> -1 then 1 else 0 end) as total_messages
              FROM cio.event_log ci
              WHERE ci.northstar_id IN", a,
              "GROUP BY
              ci.northstar_id")

qres_Q1_cio <- runQuery(q_cio, which = 'mysql')

#join cio data with data
messages_cio<-merge(x=qres_Q1_cio, y=merged_Q1_all, by ="northstar_id", all=TRUE)

messages_cio<-messages_cio%>%
  mutate(messages=as.numeric(total_messages))

mean(messages_cio$messages, na.rm=TRUE)
quantile(messages_cio$messages, na.rm=TRUE)


group_by(messages_cio, survey)%>%
  summarise(
    count = n(),
    mean = mean(messages, na.rm=TRUE),
    sd = sd(messages, na.rm=TRUE)
  )

group_by(messages_cio, nps_cat)%>%
  summarise(
    count = n(),
    mean = mean(messages, na.rm=TRUE),
    sd = sd(messages, na.rm=TRUE)
  )

ggboxplot(messages_cio, x = "nps_cat", y = "messages",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Number of messages", xlab = "NPS group")

###########################################################################
###########################################################################

######################### DESCRIPTIVE ANALYSES ############################

###########################################################################
###########################################################################

#Average NPS score
#Weighted NPS
svymean(~nps,merged_Q1_all.w)
#Unweighted NPS
mean(merged_Q1_all$nps)


####BASIC FREQUENCIES#####
#Unweighted NPS categories
prop.table(table(merged_Q1_all$nps_cat))%>%round(2)
#Weighted NPS categories
prop.table(svytable(~nps_cat, design=merged_Q1_all.w))%>%round(2)

#NPS score using Sohaib's function
nps<-getNPS(merged_Q1_all$nps,10)

#Unweighted NPS
prop.table(table(merged_Q1_all$nps))%>% round(2)
#Weighted NPS
prop.table(svytable(~nps, design=merged_Q1_all.w))%>% round(2)

#NPS unweighted averages
ddply(merged_Q1_all, .(survey), summarize, nps_sms_mean=mean(nps))
#NPS weighted averages
ddply(merged_Q1_all, .(survey), summarize, nps_sms_mean=weighted.mean(nps))

#DS value Frequencies
#Unweighted DS value categories
prop.table(table(merged_Q1_all$ds_value))%>%round(2)
#Weighted DS value categories
prop.table(svytable(~ds_value, design=merged_Q1_all.w))%>%round(2)

#Brand Perception
#IMPORTANT
#Unweighted DS value categories
prop.table(table(merged_Q1_all$important))%>%round(2)
#Weighted DS value categories
prop.table(svytable(~, design=merged_Q1_all.w))%>%round(2)

important<-merged_Q1_all %>% group_by(merged_Q1_all$important) %>%
  summarise(count = n()) %>%
  mutate(
    proportion = count/sum(count)  )


###########################################################################

############################# SATISFACTION ################################

###########################################################################

#Satisfaction - Causes

#Weighted vs. unweighted percentages
prop.table(table(merged_Q1_all$causes))%>%round(2)
prop.table(svytable(~causes, design=merged_Q1_all.w))%>%round(2)

#Weighted vs. unweighted average score
mean(merged_Q1_all$causes)
svymean(~causes,merged_Q1_all.w)

#Satisfaction - Communication

#Weighted vs. unweighted percentages
prop.table(table(merged_Q1_all$communication))%>%round(2)
prop.table(svytable(~communication, design=merged_Q1_all.w))%>%round(2)

#Weighted vs. unweighted average score
mean(merged_Q1_all$communication)
svymean(~communication,merged_Q1_all.w)

#Satisfaction - Website

#Weighted vs. unweighted percentages
prop.table(table(merged_Q1_all$website))%>%round(2)
prop.table(svytable(~website, design=merged_Q1_all.w))%>%round(2)

#Weighted vs. unweighted average score
mean(merged_Q1_all$website, na.rm=TRUE)
svymean(~website,merged_Q1_all.w, na.rm=TRUE)

#Satisfaction - Impact

#Weighted vs. unweighted
prop.table(table(merged_Q1_all$impact))%>%round(2)
prop.table(svytable(~impact, design=merged_Q1_all.w))%>%round(2)

#Weighted vs. unweighted average score
mean(merged_Q1_all$impact)
svymean(~impact,merged_Q1_all.w)

#Satisfaction - Community

#Weighted vs. unweighted
prop.table(table(merged_Q1_all$community))%>%round(2)
prop.table(svytable(~community, design=merged_Q1_all.w))%>%round(2)

#Weighted vs. unweighted average score
mean(merged_Q1_all$community)
svymean(~community,merged_Q1_all.w)

#Satisfaction - Scholarships

#Weighted vs. unweighted
prop.table(table(merged_Q1_all$scholarships))%>%round(2)
prop.table(svytable(~scholarships, design=merged_Q1_all.w))%>%round(2)

#Weighted vs. unweighted average score
mean(merged_Q1_all$scholarships)
svymean(~scholarships,merged_Q1_all.w)

#####################################################################################################

############################# IMPACT SATISFACTION PER CAMPIGN ################################

#####################################################################################################

#DoSomething About Gun Violence
gunviolence<-castAction%>%
  filter(campaign_DoSomethingAboutGunViolence>0)
#Unweighted Mean
mean(gunviolence$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
gunviolence.w<-svydesign(id = ~1, data = gunviolence, weights = gunviolence$weight)
svymean(~impact,gunviolence.w, na.rm=TRUE)

#Missing in History
MIH<-castAction%>%
  filter(campaign_MissinginHistory>0)
#Unweighted Mean
mean(MIH$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
MIH.w<-svydesign(id = ~1, data = MIH, weights = MIH$weight)
svymean(~impact,MIH.w, na.rm=TRUE)

#Thanks A Billion
ThanksBillion<-castAction%>%
  filter(campaign_ThanksaBillion>0)
#Unweighted Mean
mean(ThanksBillion$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
ThanksBillion.w<-svydesign(id = ~1, data = ThanksBillion, weights = ThanksBillion$weight)
svymean(~impact,ThanksBillion.w, na.rm=TRUE)

#Ride and Seek
RS<-castAction%>%
  filter(campaign_RideSeek>0)
#Unweighted Mean
mean(RS$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
RS.w<-svydesign(id = ~1, data = RS, weights = RS$weight)
svymean(~impact,RS.w, na.rm=TRUE)

#Shower Songs
ShowerSongs<-castAction%>%
  filter(campaign_ShowerSongs>0)
#Unweighted Mean
mean(ShowerSongs$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
ShowerSongs.w<-svydesign(id = ~1, data = ShowerSongs, weights = ShowerSongs$weight)
svymean(~impact,ShowerSongs.w, na.rm=TRUE)


#My Big Regret
BigRegret<-castAction%>%
  filter(campaign_MyBigRegret>0)
#Unweighted Mean
mean(BigRegret$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
BigRegret.w<-svydesign(id = ~1, data = BigRegret, weights = BigRegret$weight)
svymean(~impact,BigRegret.w, na.rm=TRUE)

#Thumb Wars
ThumbWars<-castAction%>%
  filter(campaign_ThumbWars>0)
#Unweighted Mean
mean(ThumbWars$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
ThumbWars.w<-svydesign(id = ~1, data = ThumbWars, weights = ThumbWars$weight)
svymean(~impact,ThumbWars.w, na.rm=TRUE)

#Treat Yo Friends
TreatFriends<-castAction%>%
  filter(campaign_TreatYoFriends>0)
#Unweighted Mean
mean(TreatFriends$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
TreatFriends.w<-svydesign(id = ~1, data = TreatFriends, weights = TreatFriends$weight)
svymean(~impact,TreatFriends.w, na.rm=TRUE)

#Mirror Messages
MirrorMessages<-castAction%>%
  filter(campaign_MirrorMessages>0)
#Unweighted Mean
mean(MirrorMessages$impact, na.rm=TRUE)
#Weighted Impact Satisfaction scores
MirrorMessages.w<-svydesign(id = ~1, data = MirrorMessages, weights = MirrorMessages$weight)
svymean(~impact,MirrorMessages.w, na.rm=TRUE)


###########################################################################

############################# REG SOURCE CROSSTAB ################################

###########################################################################

#No. Signups Frequencies
prop.table(table(merged_Q1_all$signups_cat))%>%round(2)
prop.table(svytable(~signups_cat, design=merged_Q1_all.w))%>%round(2)

signups<-table(merged_Q1_all$signups_cat, merged_Q1_all$survey)
signups

prop.table(signups,2)%>%round(2)

# prop_signups<-merged_Q1_all %>% group_by(signups_cat) %>%
#   summarise(count = n()) %>%
#   mutate(
#     proportion = count/sum(count)
#   )

#Years a Member Frequencies
prop.table(table(merged_Q1_all$years_a_member))%>%round(2)
prop.table(svytable(~years_a_member, design=merged_Q1_all.w))%>%round(2)

yrmember<-table(merged_Q1_all$years_a_member, merged_Q1_all$survey)
yrmember

prop.table(yrmember,2)%>%round(2)

# prop_years_member<-merged %>% group_by(years_a_member) %>%
#   summarise(count = n()) %>%
#   mutate(
#     proportion = count/sum(count)
#   )

##########################
#######CROSS TABS#########
##########################


# xtabs(~survey+nps_cat, data=merged_Q1_all)
# tbl<-svytable(~survey+nps_cat,merged_Q1_all.w)

nps<-table(merged_Q1_all$survey, merged_Q1_all$nps_cat)
nps

prop.table(nps,1)
prop.table(svytable(nps, design=merged_Q1_all))
prop.table(svytable(~survey+nps_cat, design=merged_Q1_all.w))


#NPS category x survey segment
CrossTable(merged_Q1_all$survey, merged_Q1_all$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#unweighted
prop.table(table(merged_Q1_all$survey,merged_Q1_all$nps_cat))

#weighted
prop.table(svytable(~nps_cat+survey,design=merged_Q1_all.w))

#NPS x survey segment
CrossTable(merged_Q1_all$nps_cat, merged_Q1_all$survey, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#DS value x survey segment
CrossTable(merged_Q1_all$ds_value, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

####################################
##### Satisfaction x Survey ########
####################################

#Average satisfaction scores by survey
ddply(merged_Q1_all, .(survey), summarize, causes_mean_Q1=mean(causes), communication_mean_Q1=mean(communication),
      website_mean_Q1=mean(website), impact_mean_Q1=mean(impact), community_mean_Q1=mean(community),
                        scholarships_mean_Q1=mean(scholarships))
#Causes x survey
CrossTable(merged_Q1_all$causes, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Community x survey
CrossTable(merged_Q1_all$community, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged_Q1_all$communication, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Website x survey
CrossTable(merged_Q1_all$website, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Scholarships x survey
CrossTable(merged_Q1_all$scholarships, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Impact x survey
CrossTable(merged_Q1_all$impact, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

####################################
##### Satisfaction x NPS ########
####################################

#Average satisfaction scores by survey

#Causes x survey
CrossTable(merged_Q1_all$causes, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Community x survey
CrossTable(merged_Q1_all$community, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Communication x survey
CrossTable(merged_Q1_all$communication, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Website x survey
CrossTable(merged_Q1_all$website, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Scholarships x survey
CrossTable(merged_Q1_all$scholarships, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Impact x survey
CrossTable(merged_Q1_all$impact, merged_Q1_all$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


########################################################
#################### RE-ENGAGEMENT ####################
########################################################
#Unweighted Re-engagement
prop.table(table(merged_Q1_all$reengage))%>%round(2)
#weighted Re-engagement
prop.table(svytable(~reengage,design=merged_Q1_all.w))%>%round(2)

#unweighted
prop.table(table(merged_Q1_all$survey,merged_Q1_all$reengage))%>%round(2)

#weighted
prop.table(svytable(~nps_cat+survey,design=merged_Q1_all.w))%>%round(2)

#Re-engagement x survey
CrossTable(merged_Q1_all$reengage, merged_Q1_all$survey, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

campaigns_binary<-castAction%>%
  mutate(
    gunviolence=ifelse(campaign_DoSomethingAboutGunViolence>0,1,0),
    missinghistory=ifelse(campaign_MissinginHistory>0,1,0),
    mirrormessages=ifelse(campaign_MirrorMessages>0,1,0),
    thanksabillion=ifelse(campaign_ThanksaBillion>0,1,0),
    rideseek=ifelse(campaign_RideSeek>0,1,0),
    showersongs=ifelse(campaign_ShowerSongs>0,1,0),
    mybigregret=ifelse(campaign_MyBigRegret>0,1,0),
    thumbwars=ifelse(campaign_ThumbWars>0,1,0),
    treatyofriends=ifelse(campaign_TreatYoFriends>0,1,0)
  )

#Re-engagement x Gun Violence
CrossTable(campaigns_binary$reengage, campaigns_binary$campaign_DoSomethingAboutGunViolence, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Mirror Messages
CrossTable(campaigns_binary$reengage, campaigns_binary$mirrormessages, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Missing in History
CrossTable(campaigns_binary$reengage, campaigns_binary$missinghistory, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Thanks A Billion
CrossTable(campaigns_binary$reengage, campaigns_binary$thanksabillion, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Ride and Seek
CrossTable(campaigns_binary$reengage, campaigns_binary$rideseek, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Shower Songs
CrossTable(campaigns_binary$reengage, campaigns_binary$showersongs, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x My Big Regret
CrossTable(campaigns_binary$reengage, campaigns_binary$mybigregret, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Treat Yo Friends
CrossTable(campaigns_binary$reengage, campaigns_binary$treatyofriends, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Re-engagement x Treat Yo Friends
CrossTable(campaigns_binary$reengage, campaigns_binary$thumbwars, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


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

############################################
####### LOGISTIC REGRESSION MODELS #########
############################################

#create binary variables
logisitic<-castAction%>%
  mutate(
    community_rec=(ifelse(community==5,1,0)),
    causes_rec=(ifelse(causes==5,1,0)),
    website_rec=(ifelse(website==5,1,0)),
    communication_rec=(ifelse(communication==5,1,0)),
    scholarships_rec=(ifelse(scholarships==5,1,0)),
    impact_rec=ifelse(impact==5,1,0),
    gunviolence=ifelse(campaign_DoSomethingAboutGunViolence>0,1,0),
    missinghistory=ifelse(campaign_MissinginHistory>0,1,0),
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



logreg_promoter <-glm(promoter~missinghistory,
                      data=logisitic, family=binomial(link="logit"))

summary(logreg_promoter)
exp(coef(logreg_promoter))

# Satisfaction
regression_causes = as.formula('nps ~ causes')
satisfaction_causes <- lm(regression_causes, cause_action_types)
tidy(satisfaction_causes)


############################################
####### Sign-ups and RBs x NPS #########
############################################

CrossTable(merged_Q1_all$signups_cat, merged_Q1_all$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

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

# ##GLADITOR - COMPS###
# gladiator<-read.csv('~/Documents/Sentiment Survey/Member Sentiment Q4/Analyses/Q4 identify Sentiment respondents in Gladiator.csv')
#   # ##Merge with db data
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
CrossTable(merged_Q1_all$signups_cat, merged_Q1_all$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

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
CrossTable(merged_Q1_all$ds_value, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(merged_Q1_all$ds_value, merged_Q1_all$nps_cat,prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))



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