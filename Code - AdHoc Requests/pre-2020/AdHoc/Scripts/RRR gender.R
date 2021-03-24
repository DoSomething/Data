library(tidyverse)
library(plyr)
library(gmodels)
source('config/init.R')
source('config/mySQLConfig.R')

#import first name gender sheet
names <- read.csv('~/Desktop/Gender First Name Master File.csv')

#upcase names
names_upcase <- mutate_all(names, funs(toupper))

q <- paste0("
SELECT ca.northstar_id,
u.first_name
FROM quasar.campaign_activity ca
LEFT JOIN quasar. users u 
ON ca.northstar_id = u.northstar_id
WHERE ca.campaign_run_id IN ('8049')")


RRR <- runQuery(q, which = 'mysql')

#upcase names
RRR_upcase <- RRR%>%
  mutate(first_name = toupper(gsub("[^[:alnum:] ]", "", first_name)))

# 
# RRR_upcase <- mutate_all(RRR, funs(toupper))

#merge name file with RRR signups
names_merge<-merge(x=names_upcase, y=RRR_upcase, by ="first_name", all=TRUE)

names_merge <-names_merge %>%
  mutate(gender_rec = case_when(gender == 'MALE' ~ 'male',
                      gender == 'FEMALE' ~ 'female'),
                      gender == is.na(gender) ~ 'missing',
         noname = case_when (first_name !=is.na (first_name) ~0,
                             is.na(first_name) | first_name == "" ~ 1))
                          

CrossTable(names_merge$gender_rec, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))

