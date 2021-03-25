library(dplyr)
library(survey)
library(gmodels)

#Pull Typeform data
quarterly_nps <- read.csv('~/Documents/Sentiment Survey/Member Sentiment Q3 2018/DoSomething Sentiment Survey Q3 2018.csv')

#Create NPS categories
quarterly_nps_Q3 <- quarterly_nps%>%
  rename(nps=Considering.your..em.complete..em..experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'))

#Double check coding
#CrossTable(quarterly_nps_Q3$nps,quarterly_nps_Q3$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Get NPS score
nps_score <- getNPS(quarterly_nps_Q3$nps,10)%>%
print(nps_score)

#See percentage breakdown
prop.table(table(quarterly_nps_Q3$nps_cat))

#NPS by segment
#All other members (Email + SMS)
quarterly_nps_email <- quarterly_nps_Q3%>%
  filter(source=='email')
nps_score_email <- getNPS(quarterly_nps_email$nps,10)%>%
  print(nps_score_email)

#SMS-only Members 
quarterly_nps_sms <- quarterly_nps_Q3%>%
  filter(source=='sms')
nps_score_sms <- getNPS(quarterly_nps_sms$nps,10)%>%
print(nps_score_sms)

#See percentage breakdowns 
CrossTable(quarterly_nps_Q3$nps_cat, quarterly_nps_Q3$source, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#################################################
############# Weighted NPS ######################
#################################################
#Create table for survey respondents breakdown (% SMS-only vs. All other members)
survey_breakdown <- count(quarterly_nps_Q3, source)%>%
  mutate(survey_pct=n/sum(n))

#Create table for DS membership (40% SMS only members, 60% SMS + Email. Based on latest data https://dsdata.looker.com/looks/1192)
ds_pct <- matrix(c("email",0.60, "sms",0.40),ncol=2,byrow=TRUE)
colnames(ds_pct) <- c("source","ds_pct")
ds_pct <-as.table(ds_pct)
#Transform table into object
ds_pct <- as.data.frame.matrix(ds_pct)%>%
  mutate(ds_pct=as.numeric(ds_pct))
  
#Add ds membership % to main dataset
quarterly_nps_Q3 <- ds_pct%>%
  inner_join(quarterly_nps_Q3,by ="source")

#Join survey breakdown table with main dataset and calculate weight
weights_nps <- survey_breakdown%>%
  inner_join(quarterly_nps_Q3,by ="source")%>%
  mutate(weight=1/(survey_pct/ds_pct))

#set weights
nps.w<-svydesign(id = ~1, data = weights_nps, weights = weights_nps$weight)

#Check weighting was applied correctly (should be 60% All other, 40% SMS-only)
#Weighted 
prop.table(svytable(~source, design=nps.w))
#Unweighted (original survey respondent breakdown)
prop.table(table(weights_nps$source))

#unweighted NPS
prop.table(table(weights_nps$nps_cat))
#weighted NPS
prop.table(svytable(~nps_cat, design=nps.w))

#Unweighted Mean score
mean(weights_nps$nps)
#weighted Mean score
svymean(~nps,design=nps.w)

