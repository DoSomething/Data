#Pull SMS NPS Typeform October 24th
sms_nps_oct <- read.csv('~/Documents/NPS/SMS/SMS Q4 NPS/October 2018/SMS Q4 October pulled Nov 16.csv')

#add in NPS category
sms_nps_oct<- sms_nps_oct%>%
  rename(nps=Considering.your.complete.experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(Submit.Date..UTC., 1, 10), origin='1970-01-01'))

#remove dups
sms_nps_oct <- sms_nps_oct%>%
  filter(!duplicated(user_id))

#Get NPS score
nps_score_oct <- getNPS(sms_nps_oct$nps,10)%>%
  print(nps_score)

#Pull SMS NPS Typeform September
sms_nps_sept <- read.csv('~/Documents/NPS/SMS/SMS Q3 NPS/SMS Q3 September pulled Nov 6.csv')

#add in NPS category
sms_nps_sept<- sms_nps_sept%>%
  rename(nps=Considering.your.complete.experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date=as.Date(substr(Submit.Date..UTC., 1, 10), origin='1970-01-01'))

#remove dups
sms_nps_sept <- sms_nps_sept%>%
  filter(!duplicated(user_id))

#Get NPS score
nps_score_sept <- getNPS(sms_nps_sept$nps,10)%>%
  print(nps_score)
