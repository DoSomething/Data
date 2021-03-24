library(dtplyr)
library(tidyverse)
library(data.table)
library(lubridate)

source("Data/AdHoc/config/DSUtils.R")
source("Data/AdHoc/config/mySQLConfig.R")


# SQL to identify those who completed Quitters Always Win and Love Letters and the number of cards they made
q <- "
SELECT DISTINCT
	ca.northstar_id,
ca.campaign_run_id,
count(DISTINCT ca.quantity) AS total_cards,
max(CASE WHEN ca.campaign_run_id IN (6172) THEN 1 ELSE 0 END) AS quitters_win, 
max(CASE WHEN ca.campaign_run_id IN (7439) THEN 1 ELSE 0 END) AS love_letters
FROM quasar.campaign_activity ca
GROUP BY ca.northstar_id" 

qres<- runQuery(q)

# qres<- runQuery('Documents/Ad Hoc requests/Trello_Requests/3m cards.sql')

Cards<-qres%>%
  group_by(love_letters)
  summarise(Count=n(),
            Prop_LL = n ()/nrow(qres))

Cards <- qres%>%
  group_by(quitters_win)%>%
  summarise(mean=mean(total_cards), Count=sum(total_cards))

