library(glue)

pg <- pgConnect()

#upload SMS NPS transactional survey respondents 
sms_oct <- read.csv('~/Documents/NPS/SMS/SMS Q4 NPS/October 2018/SMS Q4 October.csv')%>%
  mutate(user_id=as.character(user_id))
#remove any duplicates
sms_oct <- sms_oct%>%
  filter(!duplicated(user_id))
#add in NPS category
sms_oct<- sms_oct%>%
  rename(nps=Considering.your.complete.experience.with.DoSomething.org..how.likely.would.you.be.to.recommend.us.to.a.friend.)%>%
  mutate(nps_cat = case_when(nps<7 ~ 'Detractor',
                             nps %in% c(7,8) ~ 'Persuadable',
                             nps>8 ~ 'Promoter'))

#Double check coding
#CrossTable(sms_oct$nps,sms_oct$nps_cat, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

#Get NPS score
nps_score <- getNPS(sms_oct$nps,10)%>%
  print(nps_score)

#Pull click actions for respondents (clicked up to two weeks before Oct 24th)
sms_actions<- glue_sql("SELECT DISTINCT bertly.northstar_id,
                       max(case when bertly.interaction_type = 'preview' then 1 else 0 end) as preview_link,
                       max(case when bertly.interaction_type = 'click' then 1 else 0 end) as click_link
                       FROM public.bertly_clicks bertly
                       WHERE (bertly.click_time >= '2018-10-10' and bertly.click_time <'2018-10-25'
                       and bertly.northstar_id in ({nsids*}))
                       group by bertly.northstar_id",
                       nsids = sms_oct$user_id,
                       .con = pg
                       )

sms_actions <- runQuery(sms_actions)

#Identify those who only had previews
sms_actions <- sms_actions%>%
  mutate(only_preview=ifelse(preview_link=='1' & click_link=='0',1,0))%>%
  rename(user_id=northstar_id)

count(sms_actions, only_preview, sort = TRUE)%>% mutate(p=n/sum(n)) 

#merge datasets
sms_oct_merged <- inner_join(sms_actions, sms_oct, by = "user_id")

#exclude respondents who only had a preview
sms_oct_nopreviews <-sms_oct_merged%>%
  filter(only_preview=='0')
#calculate NPS
nps_score <- getNPS(sms_oct_nopreviews$nps,10)%>%
  print(nps_score)
         