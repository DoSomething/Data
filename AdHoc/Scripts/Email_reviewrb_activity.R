#Load libraries
library(tidyverse)
library(data.table)
library(rmarkdown)
library(dtplyr)
library(plyr)
library(gmodels)
source('config/init.R')
source('config/mySQLConfig.R')

#import Kika's csv files from control and experiment group
email_control<-read.csv('~/Documents/Email rb status/May 2/deliveries control.csv')
email_control$group<-"control"

#identify duplicates and choose only one case
email_control<-email_control%>%
  mutate(max_opened=
           case_when(opened >0 | (is.na(opened) & !duplicated(customer_id)) ~ 1))

email_control<-email_control%>%
  filter(max_opened=='1' & !duplicated(customer_id))

email_experiment<-read.csv('~/Documents/Email rb status/May 2/deliveries.csv')
email_experiment$group<-"experiment"

#identify duplicates and choose only one case
email_experiment<-email_experiment%>%
  mutate(max_opened=
           case_when(opened >0 | (is.na(opened) & !duplicated(customer_id)) ~ 1))

email_experiment<-email_experiment%>%
  filter(max_opened=='1' & !duplicated(customer_id))

#merge files
email_all<-rbind(email_control,email_experiment)%>%
  mutate(opened_rec=ifelse(opened>0,1,0))%>%
  filter(!duplicated(customer_id))

#convert UTC timestamps to date
email_all$delivered <-as.POSIXct(email_all$delivered, origin='1970-01-01')
# email_all$created <-as.POSIXct(email_all$created, origin='1970-01-01')
# email_all$sent <-as.POSIXct(email_all$sent, origin='1970-01-01')

email_all<-email_all%>%
  select(customer_id, opened, opened_rec, group, delivered)%>%
  mutate(delivered_date = as.Date(delivered,'%Y-%m-%d %H:%M:%S'))%>%
  rename(northstar_id=customer_id)

a_email<-prepQueryObjects(email_all$northstar_id)
q_mel_email<- paste0("SELECT
                  mel.northstar_id,
                     MIN(mel.timestamp) as 'min_active_time',
                     MAX(mel.timestamp) as 'max_active_time',
                     MAX(ca.signup_created_at) as 'max_signup_time',
                     MAX(ca.submission_created_at) as 'max_rb_time'
                     FROM quasar.member_event_log mel
                     LEFT JOIN quasar.campaign_activity ca
                     ON mel.northstar_id=ca.northstar_id
                     WHERE mel.northstar_id IN", a_email,
                     "GROUP BY
                     mel.northstar_id")

email_mel <- runQuery(q_mel_email, which = 'mysql')

email_mel<-email_mel%>%
  mutate(first_date_active=as.Date(paste0(min_active_time,'%Y-%m-%d %H:%M:%S')),
         last_date_active=as.Date(paste0(max_active_time,'%Y-%m-%d %H:%M:%S')),
         last_date_signup=as.Date(paste0(max_signup_time,'%Y-%m-%d %H:%M:%S')),
         last_date_rb=as.Date(paste0(max_rb_time,'%Y-%m-%d %H:%M:%S')))

#merge cio data and mel data
email_merged<-merge(x=email_all, y=email_mel, by ="northstar_id", all=TRUE)

email_merged<-email_merged%>%
  select(northstar_id,opened_rec,group,delivered_date,last_date_active,last_date_signup,last_date_rb)

email_merged<-email_merged%>%
  mutate(days_active_since_email=last_date_active - delivered_date,
         days_signup_since_email=last_date_signup - delivered_date,
         days_rb_since_email=last_date_rb - delivered_date,
        got_email = case_when(group=='control'| group =='experiment' ~ 1,
                              TRUE ~ 0))

email_merged <- email_merged%>%
  mutate(active_after=ifelse(days_active_since_email>0,1,0),
         signedup_after=ifelse(days_signup_since_email>0,1,0),
         reportedback_after=ifelse(days_rb_since_email>0,1,0))

email_opened<-email_merged%>%
  filter(opened_rec>0)

#check quantiles of # active after email
quantile(email_opened$days_active_since_email, na.rm = TRUE)
quantile(email_opened$days_signup_since_email, na.rm = TRUE)
quantile(email_opened$days_rb_since_email, na.rm = TRUE)

email_opened <- email_opened%>%
  mutate(days_active_cat=
           case_when(days_active_since_email<0 ~ 'not active after email',
                     days_active_since_email>=0 & days_active_since_email<8 ~ 'within a week',
                     days_active_since_email>=8 ~ 'over a week'),
         days_signup_cat=
           case_when(days_signup_since_email<0 ~ 'not active after email',
                     days_signup_since_email>=0 & days_active_since_email<8 ~ 'within a week',
                     days_signup_since_email>=8 ~ 'over a week'),
         days_rb_cat=
           case_when(days_rb_since_email<0 ~ 'not active after email',
                     days_rb_since_email>=0 & days_active_since_email<8 ~ 'within a week',
                     days_rb_since_email>=8 ~ 'over a week'))

CrossTable(email_opened$active_after, email_opened$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(email_opened$signedup_after, email_opened$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(email_opened$reportedback_after, email_opened$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))


CrossTable(email_opened$days_active_cat, email_opened$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(email_opened$days_signup_cat, email_opened$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(email_opened$days_rb_cat, email_opened$group, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))



###########################################################
############### Members who rb a month before email########
###########################################################

q_mel<- paste0("SELECT
               mel.northstar_id,
               MAX(mel.timestamp) as 'max_active_time',
               MAX(ca.signup_created_at) as 'max_signup_time',
               MAX(ca.submission_created_at) as 'max_rb_time',
               MIN(ca.submission_created_at) as 'min_rb_time',
               sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
               MAX(case when ca.post_id <> -1 AND ca.submission_created_at >'2018-03-19' Then 1 ELSE 0 END) as 'rb_after_6wks',
               CASE WHEN max(ca.post_id) <> -1 THEN DATEDIFF(MIN(DATE(CASE WHEN ca.submission_created_at = '0000-00-00 00:00:00' THEN NULL ELSE 							   ca.submission_created_at END)),
               MIN(DATE(ca.submission_created_at))) ELSE NULL END AS days_after_firstrb,
               MIN(case when ca.post_id<> -1 AND ca.submission_created_at >'2018-02-01' AND ca.submission_created_at < '2018-03-19' Then 1 ELSE 0 END) as 			           'rb_month_before'
               FROM quasar.member_event_log mel
               LEFT JOIN quasar.campaign_activity ca
               ON mel.northstar_id=ca.northstar_id
               WHERE ca.signup_created_at >= '2018-01-18'
               GROUP BY
               mel.northstar_id")

rbmembers_noemail <- runQuery(q_mel, which = 'mysql')


rbmembers_noemail_rb <- rbmembers_noemail%>%
  filter(rb_month_before==1)%>%
  mutate(last_date_active=as.Date(paste0(max_active_time,'%Y-%m-%d %H:%M:%S')),
         last_date_signup=as.Date(paste0(max_signup_time,'%Y-%m-%d %H:%M:%S')),
         last_date_rb=as.Date(paste0(max_rb_time,'%Y-%m-%d %H:%M:%S')),
         first_date_rd=as.Date(paste0(min_rb_time,'%Y-%m-%d %H:%M:%S')))

rbmembers_noemail_rb <- rbmembers_noemail_rb%>%
  filter(last_date_active < '2018-03-19' & last_date_signup < '2018-03-19' & last_date_rb < '2018-03-19')

#label the same var names as email_all
rbmembers_noemail_rb <- rbmembers_noemail_rb%>%
  mutate(days_active_since_email=last_date_active - last_date_rb,
         days_signup_since_email=last_date_signup - last_date_rb,
         days_rb_since_email=last_date_rb - last_date_rb)

rbmembers_noemail_rb <- rbmembers_noemail_rb%>%
  mutate(active_after=ifelse(days_active_since_email>0,1,0),
         signedup_after=ifelse(days_signup_since_email>0,1,0),
         reportedback_after=ifelse(days_rb_since_email>0,1,0))

rbmembers_noemail_rb$got_email<-0

#Select relevant variables
rbmembers_noemail_rb <- rbmembers_noemail_rb%>%
  select(northstar_id, last_date_active, last_date_signup, last_date_rb, days_active_since_email, days_signup_since_email, days_rb_since_email,
         active_after,signedup_after,reportedback_after, got_email)

email_opened<- email_opened%>%
  select(northstar_id, last_date_active, last_date_signup, last_date_rb, days_active_since_email, days_signup_since_email, days_rb_since_email,
         active_after,signedup_after,reportedback_after, got_email)

# merge members who got rb email with members who rb one month before emaik
compare<-rbind(email_opened, rbmembers_noemail_rb)


CrossTable(compare$active_after, compare$got_email, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(compare$signedup_after, compare$got_email, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(compare$reportedback_after, compare$got_email, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

write.csv(compare, file = "compare email rb status.csv")



#import Typeform csv files for embedded NPS
# typeform_nps_control<-read.csv('~/Documents/Email rb status/Email Feedback C.csv')
# typeform_nps_control$group<-"control"
#
# typeform_nps_experiment<-read.csv('~/Documents/Email rb status/Email Feedback.csv')
# typeform_nps_experiment$group<-"experiment"
#
# #rename col names
# colnames(typeform_nps_control)<- c("typeform_id", "nps", "nps_reason", "customer_id", "start_date", "submit_date", "network", "group")
# colnames(typeform_nps_experiment)<- c("typeform_id", "nps", "nps_reason", "customer_id", "start_date", "submit_date", "network", "group")
#
# typeform_all<-rbind(typeform_nps_control,typeform_nps_experiment)%>%
#   select(customer_id, group, nps, nps_reason, submit_date)%>%
#   mutate(
#     submit_date = as.Date(paste0(submit_date,'%Y-%m-%d %H:%M:%S')))
#
# #filter to only typeforms before April 6th (Kika's file was sent on April 5th)
# typeform_all<-typeform_all%>%
#   filter(submit_date< '2018-04-06')
#
# #merge cio and typeform data
# email_merged<-merge(x=email_all, y=typeform_all, by ="customer_id", all=TRUE)
#
# #remove duplicates
# email_merged<-email_merged%>%
#   filter(!duplicated(customer_id))



