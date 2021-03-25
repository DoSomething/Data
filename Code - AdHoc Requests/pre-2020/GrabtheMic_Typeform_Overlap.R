library(tidyverse)
library(data.table)
library(openxlsx)
library(plyr)

source('config/init.R')
source('config/mySQLConfig.R')

#Query to pull db data for Grab the Mic - if they signed up and total number of report backs
q<- paste0("
SELECT
c.northstar_id,
count(DISTINCT c.signup_id) AS total_signups,
max(case when c.post_id <> -1 then 1 else 0 end) as rb,
c.url,
c.status,
c.campaign_run_id
FROM quasar.campaign_activity c
WHERE c.campaign_run_id=8022
GROUP BY c.northstar_id")

gtm_rb <- runQuery(q, which = 'mysql')

gtm_typeform <- read_excel("~/Documents/Grab the Mic/Grab_the_Mic__January-report.xlsx")
gtm_typeform$northstar_id=gtm_typeform$id
gtm_typeform$survey='1'

merged<-merge(x=gtm_typeform, y=gtm_rb, by ="northstar_id", all=TRUE)

#create variable for if they reported back
merged<-merged%>%
  mutate(
    reportedback=ifelse(rb>0,1,0),
    didboth=ifelse(survey==1 & rb>0,1,0))

#only look at those who completed the survey
merged<-merged%>%
filter(survey==1)

gtm_surveyedrb <-merged%>%
  dplyr::select(2:28,30:32,29,1,33:40)

write.csv(gtm_surveyedrb, file = "Grab the Mic Typeform and Report backs.csv")

#only look at those who completed the survey and said yes to registered to vote
registered<-merged%>%
  filter(survey==1 & merged$`Are you registered to vote?`=='Yes' & northstar_id!na)

write.csv(registered, file = "January GTB Registered to Vote.csv")

