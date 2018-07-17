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

#Import latest Question Pro data pulled on July 17th (N=3583)
# member_survey_2018 <- read.xlsx('~/Documents/Member Surveys/Member Survey 2018 /2018 Member Survey final .xlsx')

#Import member survey data from PostGres (N should equal 3538)
mem_survey <- "select *
        from survey.member_survey_2018"

member_survey_2018 <- runQuery(mem_survey)

#########################################################################
######################## DATA CLEANING ##################################
#########################################################################

#Recode race categories
collapseRace <- function(dat) {

  raceSet <- dat %>% select(questionpro_id, starts_with('race'))
  raceVars <- raceSet %>% select(starts_with('race')) %>% names()

  setRace <-
    member_survey_2018 %>%
    mutate_at(

      .vars = vars(starts_with('race_')),
      .funs = funs(ifelse(is.na(.),0,1))

    ) %>%
    mutate(

      ticks = rowSums(select(., contains("race_"))),

      race = case_when(
        ticks > 1 ~ 'Multiracial',
        get(raceVars[1])==1 & ticks==1 ~ 'White',
        get(raceVars[2])==1 & ticks==1 ~ 'Hispanic/Latino',
        get(raceVars[3])==1 & ticks==1 ~ 'Black',
        get(raceVars[4])==1 & ticks==1 ~ 'Native American',
        get(raceVars[5])==1 & ticks==1 ~ 'Asian',
        get(raceVars[6])==1 & ticks==1 ~ 'Pacific Islander',
        TRUE ~ 'Uncertain'
      )

    ) %>%
    select(-starts_with('race.'), -ticks)

}

member_survey_2018 <-collapseRace(member_survey_2018)

## Create new variables ##
member_survey_2018 <- member_survey_2018%>%
  mutate(registered_cat=
           case_when(voter_reg_status=='Are not now registered to vote' ~ 'Not registered or out of date',
                     voter_reg_status=='IÕm not sure'~ 'Dont know',
                     (voter_reg_status=='Are registered to vote at a permanent address while residing at a temporary address (e.g. school, serving in the military)' |
                        voter_reg_status=='Are registered to vote at your current address' |
                        voter_reg_status=='Are registered to vote, but your address is out of date') ~ 'Registered'),
         gender_cat=
           case_when(gender=='Woman' ~ 'Woman',
                     gender=='Man' ~ 'Man',
                     (gender=='Gender Non-conforming/Non-binary/Two-Spirit' |
                        gender=='Prefer not to say' |
                        gender=='ÊI dont see my identity represented' ~ 'Other')),
         gender_dummy=
           case_when(gender=='Woman' ~ 'Woman',
                     gender=='Man' ~ 'Man'))

member_survey_2018 <- member_survey_2018%>%
  mutate(registered_dummy=
           case_when(registered_cat=='Not registered or out of date' ~ 0,
                     registered_cat=='Registered' ~ 1),
         race_cat=
           case_when(race=='White' ~ 'race1_White',
                     race=='Black' ~ 'race2_Black',
                     race=='Asian' ~ 'race4_Asian',
                     race=='Hispanic/Latino' ~ 'race3_Hispanic/Latino',
                     race=='Multiracial' ~ 'race4_Multiracial'),
         age_dummy=
           case_when((age<18 | age =='Younger than 13') ~ 'Younger than 18',
                     age>=18 | age=='Older than 25' ~ '18 or Older'),
         gpa_rec=
           case_when(gpa=='A- to A+' ~ 'A- to A+',
                     gpa=='B- to B+' ~ 'B- to B+',
                     gpa=='C- to C+' ~ 'C- to C+'),
         volunteer_cat=
           case_when(volunteer_frequency=='More than once a week' |
                       volunteer_frequency=='About once a week'|
                       volunteer_frequency=='2-3 times a month'|
                       volunteer_frequency=='About once a month' ~ 'At least once a month',
                     volunteer_frequency=='Once every few months' |
                       volunteer_frequency=='One time only'~ 'Once every few months',
                     volunteer_frequency=='Never' ~ 'Never'),
         political_view_rec=
           case_when(political_view=='Conservative' | political_view=='Very conserative' ~ 'Conservative',
                     political_view=='Moderate' ~'Moderate',
                     political_view=='Liberal' | political_view=='Very liberal' ~ 'Liberal'),
         asked_ds =
           case_when(voter_reg_ask_dosomething=='Asked by DoSomething.org' ~ 1,
                     voter_reg_ask_dosomething=is.na(voter_reg_ask_dosomething) ~ 0),
         nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter')
         )

####################################################
#################### ANALYSES ######################
####################################################

#Remove duplicates and rename nsid to northstar_id
membersurvey_dedup <- member_survey_2018%>%
  filter(!duplicated(nsid))%>%
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

merged_member_survey$created_at <-as.Date(merged_member_survey$created_at, '%Y-%m-%d %H:%M:%S')

#Create Niche category
merged_member_survey <- merged_member_survey%>%
  mutate(last_signup_date = as.Date(last_signup, '%Y-%m-%d %H:%M:%S'),
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

 #Look at NPS scores for only respondents with sign ups in last year and no missing NPS scores
 nps<-nps%>%
   filter(last_signup_date>'2017-06-13' & nps_cat!=is.na(nps_cat))%>%
   select(northstar_id, source, source_niche, nps, nps_cat, last_signup_date, source_sms,birthdate, source_segment)

 #NPS breakdown by channel
 CrossTable(nps$source_segment, nps$nps_cat, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
 #NPS breakdown
 CrossTable(nps$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
 nps_score <- getNPS(nps$nps,10)
 nps_score

 #Membership Breakdown
 getPopBreakdown <- function() {
   breakdown <- paste0("
                       SELECT count(*) AS total_active,
                       SUM(CASE WHEN cio_status <> 'subscribed' AND sms_status IN ('less','pending','active') THEN 1 ELSE 0 END) AS sms_only,
                       SUM(CASE WHEN source='niche' THEN 1 ELSE 0 END) AS niche
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
     setNames(c('group','n')) %>%
     mutate(p=n/sum(n))

   return(pop)
 }

 mem_pop <-getPopBreakdown()

 #Create engaged Niche group and Set survey weights so they match DS membership breakdown. Hard coded weights, ref: https://docs.google.com/document/d/19fg-9-UQpXn47eOsV7ljr5o9TB0Qurnf8TJEri9gQcE/edit
 nps<-nps %>%
   mutate(
     weight_survey=
       case_when(
         source_segment=='Niche' ~ 1.263332932,
         source_segment=='SMS only' ~ 0.609513782,
         source_segment=='Typical' ~ 1.127987058,
         source_segment=is.na(source_segment) ~ 1)
   )

 #set weights
 nps.w<-svydesign(id = ~1, data = nps, weights = nps$weight_survey)

 #Check weighting
 #Weighted Reg Source
 prop.table(svytable(~source_segment, design=nps.w))
 #Unweighted Reg Source (original survey respondents)
 prop.table(table(nps$source_segment))

 #unweighted NPS
 prop.table(table(nps$nps_cat))
 #weighted NPS
 prop.table(svytable(~nps_cat, design=nps.w))


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
            SELECT
            u.northstar_id, u.email,u.source,u.cio_status,
            CASE WHEN u.source = 'niche'
            AND u.birthdate >= '{age25*}'
            AND u.birthdate < '{age13*}'
            THEN 1 ELSE 0 END as niche,
            CASE WHEN u.cio_status <> 'subscribed' AND u.sms_status IN ('less','pending','active') THEN 1 ELSE 0 END AS sms_only,
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


#Get earliest signup date
# minDate <- min(as.Date(substr(sample$signup_created_at, 1, 10)))

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
  mutate(weight_sampling = prob*weight_survey)

niche_weighted <-weighted_members_nps %>%
  select(northstar_id, source_segment, weight_sampling, nps, first_signup_date,last_signup_date.x,avg_signup_date, prob)%>%
  filter(source_segment=='Niche')

scores <- numeric()
for (i in 1:1000) {
  niche_weighted_nps <-niche_weighted%>%
    sample_n(nrow(.), weight = prob, replace = T)
  score <- getNPS(niche_weighted$nps,10)
  scores <- c(scores, score)
}

score

sms_weighted <-weighted_members_nps%>%
  filter(source_segment=='SMS only')%>%
  select(northstar_id, source_segment, weight_sampling, nps, first_signup_date,last_signup_date.x,avg_signup_date, prob)

scores <- numeric()
for (i in 1:1000) {
  sms_weighted_nps <-sms_weighted%>%
  sample_n(nrow(.), weight = prob, replace = T)
  score <- getNPS(sms_weighted$nps,10)
  scores <- c(scores, score)
}

score

typical_weighted <-weighted_members_nps %>%
  filter(source_segment=='Typical')%>%
  select(northstar_id, source_segment, weight_sampling, nps, first_signup_date,last_signup_date.x,avg_signup_date, prob)

scores <- numeric()
for (i in 1:1000) {
  typical_weighted_nps <-typical_weighted%>%
    sample_n(nrow(.), weight = prob, replace = T)
  score <- getNPS(typical_weighted$nps,10)
  scores <- c(scores, score)
}

score

scores <- numeric()
for (i in 1:1000) {
  all_weighted_nps <- weighted_members_nps%>%
    sample_n(nrow(.), weight = weight_sampling, replace = T)
  score <- getNPS(weighted_members_nps$nps,10)
  scores <- c(scores, score)
}

score

#####charts #########
sampleNiche <-
  niche_weighted %>%
  sample_n(nrow(.), replace = F, weight = prob)

ggplot(sampleNiche, aes(x=last_signup_date.x, y=nps)) +
  geom_point() + geom_smooth() + ggtitle('Niche') +
  scale_x_date(breaks=pretty_breaks(5))

sampleSMS <-
  sms_weighted %>%
  sample_n(nrow(.), replace = F, weight = prob)

ggplot(sampleSMS, aes(x=last_signup_date.x, y=nps)) +
  geom_point() + geom_smooth() + ggtitle('SMS Only') +
  scale_x_date(breaks=pretty_breaks(5))

sampleRegular <-
  typical_weighted %>%
  sample_n(nrow(.), replace = F, weight = prob)

ggplot(sampleRegular, aes(x=last_signup_date.x, y=nps)) +
  geom_point() + geom_smooth() + ggtitle('Typical') +
  scale_x_date(breaks=pretty_breaks(5))


sampleAll <-
  weighted_members_nps %>%
  sample_n(nrow(.), replace = F, weight = weight_sampling)

ggplot(sampleAll, aes(x=last_signup_date.x, y=nps)) +
  geom_point() + geom_smooth() + ggtitle('All members') +
  scale_x_date(breaks=pretty_breaks(5))

##################################################################################
######################## Compare Gender ##########################################
##################################################################################

count(membersurvey_dedup, gender, sort = TRUE)%>% filter(gender!=is.na(gender))%>% mutate(p=n/sum(n))
count(membersurvey_dedup, gender_dummy, sort = TRUE)%>% filter(gender_dummy!=is.na(gender_dummy))%>%mutate(p=n/sum(n))

#import list of names
names <- read.csv('~/Desktop/Gender First Name Master File.csv')
#upcase names
names <- mutate_all(names, funs(toupper))


#pull first names of survey respondents from db
db_names <- members_postgres%>%
  select(northstar_id,first_name)
#upcase names
db_names <- db_names%>%
  mutate(first_name = toupper(gsub("[^[:alnum:] ]", "", first_name)))


#join names from list to survey respondents
names_respondents<- db_names%>%
 left_join(names, by='first_name')

#Compare frequencies of gender from db to seld-reported gender
count(names_respondents, gender, sort = TRUE)%>% mutate(p=n/sum(n))
count(names_respondents, gender, sort = TRUE)%>%filter(gender!=is.na(gender))%>% mutate(p=n/sum(n))


###############################################################################
################################ VOTER REG ####################################
###############################################################################

#freqs
count(membersurvey_dedup, voter_reg_status, sort = TRUE)%>%filter(voter_reg_status!=is.na(voter_reg_status))%>% mutate(p=n/sum(n))
count(membersurvey_dedup, registered_cat, sort = TRUE)%>%filter(registered_cat!=is.na(registered_cat))%>% mutate(p=n/sum(n))

# count(membersurvey_dedup, asked_ds, sort = TRUE)%>% mutate(p=n/sum(n))

#count(membersurvey_dedup, gender_dummy, sort = TRUE)%>% filter(gender_dummy!=is.na(gender_dummy))%>% mutate(p=n/sum(n))
#count(membersurvey_dedup, gpa, sort = TRUE)%>% filter(gpa!=is.na(gpa))%>% mutate(p=n/sum(n))
#count(membersurvey_dedup, age, sort = TRUE)%>% filter(age!=is.na(age))%>% mutate(p=n/sum(n))
#count(membersurvey_dedup, sub_urb, sort = TRUE)%>% filter(sub_urb!=is.na(sub_urb))%>% mutate(p=n/sum(n))

#Exploring
CrossTable(membersurvey_dedup$gender_dummy, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(membersurvey_dedup$fam_finances, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(membersurvey_dedup$sub_urb, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(membersurvey_dedup$political_view_rec, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(membersurvey_dedup$political_party, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(membersurvey_dedup$contacted_official, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(membersurvey_dedup$race_cat, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#CrossTable(membersurvey_dedup$gender_dummy, membersurvey_dedup$registered_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(membersurvey_dedup$gender_dummy, membersurvey_dedup$registered_dummy, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(member_survey_2018$registered_cat, member_survey_2018$sub_urb, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
#CrossTable(member_survey_2018$registered_cat, member_survey_2018$gender_dummy, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


#Logistic regression model - RACE
logreg_registered <-glm(registered_dummy~race_cat,
                        data=membersurvey_dedup, family=binomial(link="logit"))

summary(logreg_registered)
exp(coef(logreg_registered))


#Logistic regression model - GENDER
logreg_registered_gender <-glm(registered_dummy~gender_dummy,
                               data=membersurvey_dedup, family=binomial(link="logit"))

summary(logreg_registered_gender)
exp(coef(logreg_registered_gender))


#Logistic regression model - POL PARTY
logreg_registered_polparty <-glm(registered_dummy~political_party,
                        data=membersurvey_dedup, family=binomial(link="logit"))

summary(logreg_registered_polparty)
exp(coef(logreg_registered_polparty))


