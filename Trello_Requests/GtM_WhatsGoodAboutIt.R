#Trello data request https://trello.com/c/vg2d6q9e/1264-grab-the-mic-whats-good-about-it

library(tidyverse)
library(data.table)
library(rmarkdown)
library(plyr)
library(gmodels)


source('config/init.R')
source('config/mySQLConfig.R')
source('Q1/Scripts/getSampledNPS.R')

# Pull data for GtM return to site more times than avg campaign and rb rate
q_gtm<- paste0("SELECT
          c.northstar_id,
           sum(case when c.post_id <> -1 then 1 else 0 end) as reported_backs,
           sum(case when ul.last_accessed >= '2018-01-01' AND ul.last_accessed <= '2018-03-31' then 1 else 0 end ) as site_visits
           FROM quasar.campaign_activity c
           LEFT JOIN quasar.users_log ul ON c.northstar_id=ul.northstar_id
           LEFT JOIN quasar.users u ON c.northstar_id=u.northstar_id
           WHERE c.campaign_run_id = 8022 AND u.email NOT LIKE '%dosomething.org%'
           GROUP BY c.northstar_id")

gtm <- runQuery(q_gtm, which = 'mysql')

gtm$group<-"gtm"

# Pull data for Feeing Better Futures return to site more times than avg campaign and rb rate
q_gtm_feeding<- paste0("SELECT 
                        c.northstar_id,
                       sum(case when c.post_id <> -1 then 1 else 0 end) as reported_backs,
                       sum(case when ul.last_accessed >= '2018-01-01' AND ul.last_accessed <= '2018-03-31' then 1 else 0 end ) as site_visits
                       FROM quasar.campaign_activity c 
                       LEFT JOIN quasar.users_log ul ON c.northstar_id=ul.northstar_id
                       LEFT JOIN quasar.users u ON c.northstar_id=u.northstar_id
                       WHERE c.campaign_run_id = 7985 AND u.email NOT LIKE '%dosomething.org%'
                       GROUP BY c.northstar_id ")

feeding <- runQuery(q_gtm_feeding, which = 'mysql')

feeding$group<-"feeding better futures"

# Pull data for Team Up Dream Up return to site more times than avg campaign and rb rate
q_gtm_teamup<- paste0("SELECT 
                       c.northstar_id,
                       sum(case when c.post_id <> -1 then 1 else 0 end) as reported_backs,
                       sum(case when ul.last_accessed >= '2018-01-01' AND ul.last_accessed <= '2018-03-31' then 1 else 0 end ) as site_visits
                       FROM quasar.campaign_activity c 
                       LEFT JOIN quasar.users_log ul ON c.northstar_id=ul.northstar_id
                       LEFT JOIN quasar.users u ON c.northstar_id=u.northstar_id
                       WHERE c.campaign_run_id = 8020 AND u.email NOT LIKE '%dosomething.org%'
                       GROUP BY c.northstar_id ")

teamup <- runQuery(q_gtm_teamup, which = 'mysql')

teamup$group<-"team up dream up"

# Pull data for Save the Mascots return to site more times than avg campaign and rb rate
q_gtm_savemascots<- paste0("SELECT 
                      c.northstar_id,
                      sum(case when c.post_id <> -1 then 1 else 0 end) as reported_backs,
                      sum(case when ul.last_accessed >= '2018-01-01' AND ul.last_accessed <= '2018-03-31' then 1 else 0 end ) as site_visits
                      FROM quasar.campaign_activity c 
                      LEFT JOIN quasar.users_log ul ON c.northstar_id=ul.northstar_id
                      LEFT JOIN quasar.users u ON c.northstar_id=u.northstar_id
                      WHERE c.campaign_run_id = 8039 AND u.email NOT LIKE '%dosomething.org%'
                      GROUP BY c.northstar_id ")

savemascots <- runQuery(q_gtm_savemascots, which = 'mysql')

savemascots$group<-"save the mascots"

#Merge campaigns
gtm_trello<-rbind(gtm,savemascots,teamup,feeding)

#remove duplicates (members signed up across campaigns)
gtm_trello<-gtm_trello%>%
  filter(!duplicated(northstar_id))

gtm_trello<-gtm_trello%>%
  mutate(
    reportedback=ifelse(reported_backs>0,'yes','no'),
    visitedsite=ifelse(site_visits>0, 'yes', 'no')
  )

CrossTable(gtm_trello$group, gtm_trello$reportedback, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE, format= c("SPSS"))
CrossTable(gtm_trello$group, gtm_trello$visitedsite, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE, format= c("SPSS"))

gtm_onlysite<-gtm_trello%>%
  filter(site_visits>0 & site_visits<10)

quantile(gtm_onlysite$site_visits)

#Average number of site visits
group_by(gtm_trello, group) %>%
  summarise(
    count = n(),
    mean = mean(site_visits, na.rm = TRUE),
    sd = sd(site_visits, na.rm = TRUE),
    var = var(site_visits,na.rm = TRUE)
  )
