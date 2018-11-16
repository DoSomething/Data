library(tidyverse)
library(plyr)
library(gmodels)
source('config/init.R')
source('config/mySQLConfig.R')

#import first name gender sheet
names <- read.csv('~/Documents/Miscellaneous/Gender First Name Master File.csv')

#upcase names
names <- mutate_all(names, funs(toupper))

#Pull data for Friends on Lock signup and whether rb
fol <- paste0("SELECT ca.northstar_id,
              u.first_name,
              max(case when ca.post_class = 'photo - default' then 1
              else
              0 end) as reported_back
              FROM public.campaign_activity ca
              LEFT JOIN public. users u
              ON ca.northstar_id = u.northstar_id
              WHERE ca.campaign_run_id IN ('8227')
              group by 1,2")

FOL <- runQuery(fol)

#upcase names
FOL<- FOL%>%
  mutate(first_name = toupper(gsub("[^[:alnum:] ]", "", first_name)))


#merge name file with FOL signups
names_merge<-merge(x=names, y=FOL, by ="first_name", all=TRUE)

names_merge <-names_merge %>%
  mutate(gender_rec = case_when(gender == 'MALE' ~ 'male',
                      gender == 'FEMALE' ~ 'female'),
         noname = case_when (first_name !=is.na (first_name) ~0,
                             is.na(first_name) | first_name == "" ~ 1))
                          
#Gender
CrossTable(names_merge$gender_rec, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))
#RB rate by gender
CrossTable(names_merge$gender_rec, names_merge$reported_back, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))

