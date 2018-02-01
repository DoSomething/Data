library(tidyverse)
library(data.table)
library(plyr)

source('config/init.R')
source('config/mySQLConfig.R')

comp_ids<-read.csv('~/Documents/5 Days, 5 Actions/All competitors nsids.csv')
a<-prepQueryObjects(comp_ids$Users.Northstar.ID)


#Query for pulling campaigns, causes, and action types for those who completed Sentiment survey
competitors<- paste0("
SELECT
u.northstar_id,
rbs_7890.reportback_30 AS within_campaign_rbs_30,
rbs_7890.reportback_60 AS within_campaign_rbs_60,
rbs_7890.reportback_90 AS within_campaign_rbs_90,
non_7890_rbs.reportback_30 AS outside_campaign_rbs_30,
non_7890_rbs.reportback_60 AS outside_campaign_rbs_60,
non_7890_rbs.reportback_90 AS outside_campaign_rbs_90,
IFNULL(rbs_7890.reportback_30, 0) + IFNULL(non_7890_rbs.reportback_30, 0) AS total_reportbacks_30,
IFNULL(rbs_7890.reportback_60, 0) + IFNULL(non_7890_rbs.reportback_60, 0) AS total_reportbacks_60,
IFNULL(rbs_7890.reportback_90, 0) + IFNULL(non_7890_rbs.reportback_90, 0) AS total_reportbacks_90
FROM quasar.users u
LEFT JOIN
(SELECT
  rbs.northstar_id,
  sum(rbs.reportback_30) AS reportback_30,
  sum(rbs.reportback_60) AS reportback_60,
  sum(rbs.reportback_90) AS reportback_90
  FROM
  (SELECT
    c.northstar_id,
    c.signup_id,
    c.campaign_run_id,
    max(CASE WHEN c.post_id <> -1 AND c.status='accepted' AND c.submission_created_at >= '2017-08-01' AND c.submission_created_at <= '2017-08-31'  THEN 1 ELSE 0 END) AS reportback_30,
    max(CASE WHEN c.post_id <> -1 AND c.status='accepted' AND c.submission_created_at > '2017-08-31' AND c.submission_created_at <= '2017-09-30'  THEN 1 ELSE 0 END) AS reportback_60,
    max(CASE WHEN c.post_id <> -1 AND c.status='accepted' AND c.submission_created_at > '2017-09-30' AND c.submission_created_at <= '2017-10-31'  THEN 1 ELSE 0 END) AS reportback_90
    FROM quasar.campaign_activity c
    GROUP BY c.northstar_id, c.signup_id) rbs
  WHERE rbs.campaign_run_id <> 7890
  GROUP BY rbs.northstar_id) non_7890_rbs
ON non_7890_rbs.northstar_id = u.northstar_id
LEFT JOIN
(SELECT
  c1.northstar_id,
  c1.signup_id,
  sum(CASE WHEN c1.post_id <> -1 AND c1.status='accepted' AND c1.submission_created_at >= '2017-08-01' AND c1.submission_created_at <= '2017-08-31' THEN 1 ELSE 0 END) AS reportback_30,
  sum(CASE WHEN c1.post_id <> -1 AND c1.status='accepted' AND c1.submission_created_at > '2017-08-31' AND c1.submission_created_at <= '2017-09-30' THEN 1 ELSE 0 END) AS reportback_60,
  sum(CASE WHEN c1.post_id <> -1 AND c1.status='accepted' AND c1.submission_created_at > '2017-09-30' AND c1.submission_created_at <= '2017-10-31' THEN 1 ELSE 0 END) AS reportback_90
  FROM quasar.campaign_activity c1
  WHERE c1.campaign_run_id = 7890
  GROUP BY c1.northstar_id
) rbs_7890
ON u.northstar_id = rbs_7890.northstar_id
WHERE u.northstar_id IN", a)

qres <- runQuery(competitors, which = 'mysql')

# competitors<-read.csv('~/Documents/5 Days, 5 Actions/5 Days, 5 Actions Competitors.csv')
# competitors$group<-"competitors"
# control<-read.csv('~/Documents/5 Days, 5 Actions/5 Days, 5 Actions Control.csv')
# control$group<-"control"

both<-rbind(competitors,control)

both<-both%>%
  mutate(
    rb_30=ifelse(total_rbs_30>0,'yes','no'),
    rb_60=ifelse(total_rbs_60>0,'yes','no'),
    rb_90=ifelse(total_rbs_90>0,'yes','no')
  )

write.csv(both, file = '5 Days, 5 Actions approved rbs.csv')

CrossTable(both$group, both$rb_30, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(both$group, both$rb_30, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(both$group, both$rb_60, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(both$group, both$rb_90, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

ddply(both, .(group), summarize, rb_30_mean=mean(total_rbs_30), rb_60_mean=mean(total_rbs_60), rb_90_mean=mean(total_rbs_90))

#T-test for statistical significance
t.test(both$total_rbs_30~both$group)
t.test(both$total_rbs_60~both$group)
t.test(both$total_rbs_90~both$group)
t.test(both$total_rbs_120~both$group)

