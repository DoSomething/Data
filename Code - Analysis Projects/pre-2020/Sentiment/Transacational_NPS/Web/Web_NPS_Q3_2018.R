#https://trello.com/c/UGGNyRwm/1380-current-web-nps-score

#Pull Typeform data
web_nps <- read.csv('~/Downloads/Would you take this quick and easy survey_ (5).csv')
#transform date
web_nps <- web_nps%>%
  mutate(Submit.Date..UTC.=as.Date(substr(Submit.Date..UTC., 1, 10), origin='1970-01-01'))

#Pull scores for Q3 (July-September)
web_nps_Q3 <- web_nps%>%
  filter(Submit.Date..UTC.>='2018-07-01' & Submit.Date..UTC. < '2018-10-01')%>%
  rename(nps=Considering.your..em.complete..em..experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                                    nps %in% c(7,8) ~ 'Persuadable',
                                    nps>8 ~ 'Promoter'))

#Check coding
#CrossTable(web_nps_Q3$nps,web_nps_Q3$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

nps_score <- getNPS(web_nps_Q3$nps,10)
nps_score

#Alternative 
prop.table(table(web_nps_Q3$nps_cat))

