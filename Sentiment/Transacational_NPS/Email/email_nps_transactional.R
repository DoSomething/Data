#Pull Email NPS Typeform October 24th
email_nps_oct <- read.csv('~/Documents/NPS/Email/Q3/Email Q4 pulled nov 16.csv')

#add in NPS category
email_nps_oct<- email_nps_oct%>%
  rename(nps=Considering.your.complete.experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(Submit.Date..UTC., 1, 10), origin='1970-01-01'))

#remove dups
email_nps_oct <- email_nps_oct%>%
  filter(!duplicated(user_id))

#Get NPS score
email_score_oct <- getNPS(email_nps_oct$nps,10)%>%
  print(nps_score)

#Pull Email NPS QuestionPro September
email_nps_sept <- read.csv('~/Documents/NPS/Email/Q3/Email NPS Q3 September pulled nov 16.csv')

#add in NPS category
email_nps_sept<- email_nps_sept%>%
  rename(nps=How.likely.is.it.that.you.would.recommend.DoSomething.org.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(Timestamp..MM.dd.yyyy., 1, 10), origin='1970-01-01'))

#remove dups
email_nps_sept <- email_nps_sept%>%
  filter(!duplicated(External.Reference))

#Get NPS score
email_score_sept <- getNPS(email_nps_sept$nps,10)%>%
  print(nps_score)
