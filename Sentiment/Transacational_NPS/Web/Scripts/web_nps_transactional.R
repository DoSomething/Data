#Pull Web NPS Typeform
web_nps <- read.csv('~/Documents/NPS/Web/November/Web NPS Pulled nov 16.csv')

#add in NPS category
web_nps<- web_nps%>%
  rename(nps=Considering.your..em.complete..em..experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'),
         submit_date = as.Date(paste0(Submit.Date..UTC., ':00'), '%m/%d/%y %H:%M:%S'))

#September 2018 NPS
web_nps_sept <- web_nps%>%
  filter(submit_date>='2018-09-01' & submit_date < '2018-10-01')

#Get NPS score
nps_score_sept <- getNPS(web_nps_sept$nps,10)%>%
  print(nps_score)

#October 2018 NPS
web_nps_oct <- web_nps%>%
  filter(submit_date>='2018-10-01' & submit_date < '2018-11-01')

#Get NPS score
nps_score_oct <- getNPS(web_nps_oct$nps,10)%>%
  print(nps_score)
