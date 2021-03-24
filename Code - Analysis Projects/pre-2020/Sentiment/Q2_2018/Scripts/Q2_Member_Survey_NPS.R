setwd("/Users/jli/Data/Member Surveys")
source('config/init.R')
source('config/pgConnect.R')
library(naniar)
library(openxlsx)
library(glue)
library(dtplyr)
library(dplyr)
library(gmodels)
library(survey)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
pg <- pgConnect()

#Import latest Question Pro data pulled on August 3rd (N=3583)
#member_survey_2018 <- read.csv('~/Documents/Member Surveys/Member Survey 2018 /2018 Member Survey final Aug 3.csv')

#Import member survey data from PostGres (N should equal 3538)
mem_survey <- "select *
        from survey.member_survey_2018"

member_survey_2018 <- runQuery(mem_survey)

####################################################
#################### ANALYSES ######################
####################################################

## Create NPS category variable ##
member_survey_2018 <- member_survey_2018%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter')
  )

#Remove duplicates and rename nsid to northstar_id. Remove NA nsids.
membersurvey_dedup <- member_survey_2018%>%
  filter(!duplicated(nsid) & !is.na(nsid))%>%
  rename(northstar_id=nsid)

#Pull member data from database
members <- glue_sql("select u.northstar_id,
                    u.first_name,
                    u.created_at,
                    u.country,
                    u.source,
                    u.last_logged_in,
                    u.birthdate,
                    MAX(ca.signup_created_at) as last_signup,
                    count(DISTINCT ca.signup_id) as total_signups,
                    sum(case when ca.post_id <> -1 then 1 else 0 end) as total_rbs
                    from public.users u
                    left join public.campaign_activity ca
                    on u.northstar_id=ca.northstar_id
                    where u.northstar_id in ({nsids*})
                    group by 1,2,3,4,5,6,7",
                    nsids = membersurvey_dedup$northstar_id,
                    .con = pg
)

members_postgres <- runQuery(members)

#Merge Member Survey data with db data
merged_member_survey <-merge(x=membersurvey_dedup,y=members_postgres, by ="northstar_id", all=TRUE)

merged_member_survey$created_at <-as.Date(merged_member_survey$created_at, format='%Y-%m-%d %H:%M:%S')

#Create Niche category
merged_member_survey <- merged_member_survey%>%
  mutate(last_signup_date = as.Date(last_signup, format='%Y-%m-%d %H:%M:%S'),
         source_niche=ifelse(source=='niche','Niche','Not Niche'),
         source_sms=case_when(questionpro_customvar1=='sms' ~ 'sms-only',
                              questionpro_customvar1=is.na(questionpro_customvar1) ~ 'other'))

#check coding of sources
CrossTable(merged_member_survey$source,merged_member_survey$source_niche, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(merged_member_survey$questionpro_customvar1,merged_member_survey$source_sms, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#look at average sign ups to see if in last year
 ggplot(merged_member_survey, aes(x=last_signup)) + geom_density()


 #######################################################
 ################## UNWEIGHTED NPS ##################
 #######################################################

 # Create segments
 nps <-merged_member_survey%>%
   mutate(source_segment=
            case_when(source=='niche' ~ 'Niche',
                      questionpro_customvar1=='sms' ~ 'SMS only',
                      (source !='niche' | questionpro_customvar1!='sms') ~ 'Typical'))

 #check coding of segments
 # CrossTable(nps$source_sms, nps$source_segment, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
 # CrossTable(nps$source, nps$source_segment, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
 # CrossTable(nps$source_segment, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

 #Look at NPS scores for only respondents with sign ups in last year and no missing NPS scores (leave in respondents who might not have completed the entire survey)
 nps<-nps%>%
   filter(last_signup_date>'2017-06-13' & nps_cat!=is.na(nps_cat))%>%
   select(northstar_id, source, source_niche, nps, nps_cat, last_signup_date, source_sms,birthdate, source_segment, time_submitted, ds_satisfaction_causes:ds_satisfaction_campaigns, ds_value)

 #NPS breakdown by channel
 CrossTable(nps$source_segment, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
 #NPS breakdown
 CrossTable(nps$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
 nps_score <- getNPS(nps$nps,10)
 nps_score

 #######################################################
 ################## CREATE WEIGHTING ##################
 #######################################################

 #Membership Breakdown
 getPopBreakdown <- function() {
   breakdown <- paste0("
                       SELECT count(*) AS total_active,
                       COUNT(DISTINCT (CASE WHEN u.sms_status IN ('active', 'less', 'pending') AND (u.cio_status <> 'customer_subscribed' OR u.cio_status IS NULL)
                       THEN u.northstar_id else null end)) AS SMS_only,
                       COUNT(DISTINCT(CASE WHEN u.source='niche' THEN u.northstar_id ELSE NULL END)) AS Niche
                       FROM public.users u
                       WHERE (u.subscribed_member = true)
                       AND (u.email NOT LIKE '%dosomething.org' OR u.email IS NULL OR u.email = '')"
   )

   mem_breakdown <- runQuery(breakdown)

    pop <-
     mem_breakdown %>%
     mutate(
       other = total_active-sms_only-niche
     ) %>%
     melt() %>%
     filter(variable!='total_active') %>%
     setNames(c('source_segment','n')) %>%
     mutate(ds_pct=n/sum(n))

   return(pop)
 }

 mem_pop <-getPopBreakdown()

 #rename segments to match nps table
 mem_pop <- mem_pop%>%
   mutate(source_segment=
            case_when(source_segment=='niche' ~ 'Niche',
                      source_segment=='sms_only' ~ 'SMS only',
                      source_segment=='other' ~ 'Typical'))

 #Set survey weights so they match DS membership breakdown.

 #Breakdown for Survey responses
 survey_breakdown <- count(nps, source_segment)%>%
   mutate(survey_pct=n/sum(n))

#Join ds membership breakdown with survey breakdown tables and calculate weight
weights_nps <- survey_breakdown%>%
   inner_join(mem_pop,by ="source_segment")%>%
  select(source_segment,survey_pct, ds_pct)%>%
  mutate(weight=1/(survey_pct/ds_pct))

weights_nps <-weights_nps%>%
  select(source_segment,weight)

#Add weights to dataset
nps <- nps%>%
   inner_join(weights_nps,by ="source_segment")

 #set weights
 nps.w<-svydesign(id = ~1, data = nps, weights = nps$weight)

 #Check weighting
 #Weighted Reg Source
 prop.table(svytable(~source_segment, design=nps.w))
 #Unweighted Reg Source (original survey respondents)
 prop.table(table(nps$source_segment))

 #unweighted NPS
 prop.table(table(nps$nps_cat))
 #weighted NPS
 prop.table(svytable(~nps_cat, design=nps.w))

 #Unweighted Mean score
 mean(nps$nps)
#weighted Mean score
 svymean(~nps,design=nps.w)

 ##############################################################
 ############### WEIGHTING TO SAMPLING ########################
 ##############################################################


#Set random number generator seed so same observations are picked every time
set.seed(57)
prodrun=T

#Paramaters to be inserted into query. Set sign up date in last year since 2017-06-13 (first day of email invite)
yearAgo <- Sys.Date() - 365
age13 <- Sys.Date() - (365.25*13)
age25 <- Sys.Date() - (365.25*25)

sampled <- glue_sql("
            SELECT u.northstar_id, u.email,u.source,u.cio_status,
            CASE WHEN u.source = 'niche'
            AND u.birthdate >= '{age25*}'
            AND u.birthdate < '{age13*}'
            THEN 1 ELSE 0 END as niche,
            CASE WHEN (u.cio_status <> 'customer_subscribed' OR u.cio_status IS NULL) AND u.sms_status IN ('less','pending','active') THEN 1 ELSE 0 END AS sms_only,
            c.signup_created_at
            FROM public.users u
            INNER JOIN public.campaign_activity c ON c.northstar_id = u.northstar_id
            WHERE u.country = 'US'
            AND c.signup_created_at >= '2017-06-13'
            AND u.northstar_id in ({nsids*})",
                    .con = pg,
                    nsids = membersurvey_dedup$northstar_id,
                    age25 = age25,
                    age13 = age13
)

sample <-runQuery(sampled)

#Look at average sign up dates
avgSignup <-
  sample %>%
  group_by(northstar_id) %>%
  summarise(avg_signup = mean(signup_created_at))

ggplot(avgSignup, aes(x=avg_signup)) +
  geom_density() + labs(x='Average Signup Date', title = 'Average Signup Date of Respondents')

#Look at last sign up dates
LastSignup <-
  sample %>%
  group_by(northstar_id) %>%
  summarise(last_signup = max(signup_created_at))

ggplot(LastSignup, aes(x=last_signup)) +
  geom_density() + labs(x='Last Signup Date', title = 'Last Signup Date of Respondents')


#Get earliest signup date
minDate <- min(as.Date(substr(sample$signup_created_at, 1, 10)))

#Group by northstar and get the average signup date per northstar
#This required removing the time component with substr, turning it into a date,
#then turning it into a number to be able to take the mean, then turning it
#back into a date
dat <-
  sample %>%
  group_by(northstar_id) %>%
  summarise(
    niche = max(niche),
    sms_only = max(sms_only),
    avg_signup_date = as.Date(mean(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01'),
    first_signup_date = as.Date(min(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01'),
    last_signup_date = as.Date(max(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01')
  ) %>%
  #This places the signup dates on a 1:max axis
  #scalerange converts that into a var that ranges from 0:1
  mutate(
    dateCounter = as.numeric(avg_signup_date) - as.numeric(minDate) + 1,
    scaleDates = scalerange(dateCounter),
    twoMonthsAgo=Sys.Date()-60,
    twoweeks=Sys.Date()-14
  )

#Calculate the point that we want to be the fattest part of the distribution
peak <-
  dat %>%
  filter(avg_signup_date==twoMonthsAgo) %>%
  dplyr::select(scaleDates) %>% unique() %>% as.numeric()

#Remove recent signups in last 2 weeks
dat %<>%
  filter(first_signup_date < twoweeks) %>%
  #Likelihood of being selected is 1 - the distance from the peak so the closer
  #it is to the peak, the more likely it is to be selected
  mutate(
    prob = 1 - abs(scaleDates - peak)
  ) %>%
  mutate(id=northstar_id,
         distance = abs(avg_signup_date - twoMonthsAgo))

#Merge Member Survey data with db data so you can use source_segment created in NPS section (more reliable to use than membership db source code)
weighted_members_nps <- dat%>%
  inner_join(nps,by ="northstar_id")%>%
  mutate(weight_sampling = prob*weight)

niche_weighted <-weighted_members_nps %>%
  select(northstar_id, source_segment, weight_sampling, nps, first_signup_date,last_signup_date.x,avg_signup_date, prob, nps_cat)%>%
  filter(source_segment=='Niche')

scores <- numeric()
for (i in 1:1000) {
  niche_weighted_nps <-niche_weighted%>%
    sample_n(nrow(.), weight = prob, replace = T)
  score <- getNPS(niche_weighted$nps,10)
  scores <- c(scores, score)
}

mean(score)

sms_weighted <-weighted_members_nps%>%
  filter(source_segment=='SMS only')%>%
  select(northstar_id, source_segment, weight_sampling, nps, first_signup_date,last_signup_date.x,avg_signup_date, prob, nps_cat)

scores <- numeric()
for (i in 1:1000) {
  sms_weighted_nps <-sms_weighted%>%
  sample_n(nrow(.), weight = prob, replace = T)
  score <- getNPS(sms_weighted$nps,10)
  scores <- c(scores, score)
}

mean(score)

typical_weighted <-weighted_members_nps %>%
  filter(source_segment=='Typical')%>%
  select(northstar_id, source_segment, weight_sampling, nps, first_signup_date,last_signup_date.x,avg_signup_date, prob, nps_cat)

scores <- numeric()
for (i in 1:1000) {
  typical_weighted_nps <-typical_weighted%>%
    sample_n(nrow(.), weight = prob, replace = T)
  score <- getNPS(typical_weighted$nps,10)
  scores <- c(scores, score)
}

mean(score)

scores <- numeric()
for (i in 1:1000) {
  all_weighted_nps <- weighted_members_nps%>%
    sample_n(nrow(.), weight = weight_sampling, replace = T)
  score <- getNPS(weighted_members_nps$nps,10)
  scores <- c(scores, score)
}

mean(score)

#####charts #########
sampleNiche <-
  niche_weighted %>%
  sample_n(nrow(.), replace = F, weight = prob)

ggplot(sampleNiche, aes(x=last_signup_date.x, y=nps)) + ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_point() + geom_smooth() + ggtitle('Niche Members') +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b-%y")) + labs(x='Last Campaign Signup Date', y= 'Mean NPS Rating (out of 10)')

sampleSMS <-
  sms_weighted %>%
  sample_n(nrow(.), replace = F, weight = prob)

ggplot(sampleSMS, aes(x=last_signup_date.x, y=nps)) + ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_point() + geom_smooth() + ggtitle('SMS-Only Members') +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b-%y")) + labs(x='Last Campaign Signup Date', y= 'Mean NPS Rating (out of 10)')

sampleRegular <-
  typical_weighted %>%
  sample_n(nrow(.), replace = F, weight = prob)

ggplot(sampleRegular, aes(x=last_signup_date.x, y=nps)) + ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_point() + geom_smooth() + ggtitle('All Other Members') +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b-%y")) + labs(x='Last Campaign Signup Date', y= 'Mean NPS Rating (out of 10)')


sampleAll <-
  weighted_members_nps %>%
  sample_n(nrow(.), replace = F, weight = weight_sampling)

ggplot(sampleAll, aes(x=last_signup_date.x, y=nps)) + ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_point() + geom_smooth() + ggtitle('All members') +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%b-%y")) + labs(x='Last Campaign Signup Date', y= 'Mean NPS Rating (out of 10)')


######################################################
###################Satisfaction ######################
######################################################
#Satisfaction - Causes
#Weighted vs. unweighted percentages
mean(nps$ds_satisfaction_causes, na.rm=TRUE)
#Weighted vs. unweighted average score
svymean(~ds_satisfaction_causes,design=nps.w, na.rm=TRUE)

#Satisfaction - Communication
#Weighted vs. unweighted percentages
mean(nps$ds_satisfaction_communication, na.rm=TRUE)
#Weighted vs. unweighted average score
svymean(~ds_satisfaction_communication,design=nps.w, na.rm=TRUE)

#Satisfaction - Website
#Weighted vs. unweighted percentages
mean(nps$ds_satisfaction_website, na.rm=TRUE)
#Weighted vs. unweighted average score
svymean(~ds_satisfaction_website,design=nps.w, na.rm=TRUE)

#Satisfaction - Impact
#Weighted vs. unweighted percentages
mean(nps$ds_satisfaction_campaigns, na.rm=TRUE)
#Weighted vs. unweighted average score
svymean(~ds_satisfaction_campaigns,design=nps.w, na.rm=TRUE)

#Satisfaction - Community
#Weighted vs. unweighted percentages
mean(nps$ds_satisfaction_community, na.rm=TRUE)
#Weighted vs. unweighted average score
svymean(~ds_satisfaction_community,design=nps.w, na.rm=TRUE)

#Satisfaction - Scholarships
#Weighted vs. unweighted percentages
mean(nps$ds_satisfaction_scholarships, na.rm=TRUE)
#Weighted vs. unweighted average score
svymean(~ds_satisfaction_scholarships,design=nps.w, na.rm=TRUE)

# Percentage 5 stars for each area
count(nps, ds_satisfaction_causes, sort = TRUE)%>% filter(ds_satisfaction_causes!=is.na(ds_satisfaction_causes))%>%mutate(p=n/sum(n))
count(nps, ds_satisfaction_communication, sort = TRUE)%>% filter(ds_satisfaction_communication!=is.na(ds_satisfaction_communication))%>%mutate(p=n/sum(n))
count(nps, ds_satisfaction_website, sort = TRUE)%>% filter(ds_satisfaction_website!=is.na(ds_satisfaction_website))%>%mutate(p=n/sum(n))
count(nps, ds_satisfaction_scholarships, sort = TRUE)%>% filter(ds_satisfaction_scholarships!=is.na(ds_satisfaction_scholarships))%>%mutate(p=n/sum(n))
count(nps, ds_satisfaction_community, sort = TRUE)%>% filter(ds_satisfaction_community!=is.na(ds_satisfaction_community))%>%mutate(p=n/sum(n))
count(nps, ds_satisfaction_campaigns, sort = TRUE)%>% filter(ds_satisfaction_campaigns!=is.na(ds_satisfaction_campaigns))%>%mutate(p=n/sum(n))

#NPS for each area x Satisfaction star rating
#CrossTable(nps$ds_satisfaction_causes, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(nps$ds_satisfaction_communication, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(nps$ds_satisfaction_website, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(nps$ds_satisfaction_scholarships, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(nps$ds_satisfaction_community, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(nps$ds_satisfaction_campaigns, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

############################################
####### LOGISTIC REGRESSION MODEL #########
############################################

#create binary variables
logisitic<-nps%>%
  mutate(
    community_rec=(ifelse(ds_satisfaction_community==5,1,0)),
    causes_rec=(ifelse(ds_satisfaction_causes==5,1,0)),
    website_rec=(ifelse(ds_satisfaction_website==5,1,0)),
    communication_rec=(ifelse(ds_satisfaction_communication==5,1,0)),
    scholarships_rec=(ifelse(ds_satisfaction_scholarships==5,1,0)),
    impact_rec=ifelse(ds_satisfaction_campaigns==5,1,0),
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
