library(gmodels)
library(dplyr)
library(scales)
library(glue)


#Pull nsids for those who signed up for Untangle the Web
untangle_signups <- ("SELECT signups.northstar_id
                      FROM public.signups signups
                      WHERE signups.campaign_id='9003'")

untangle_signups <- runQuery(untangle_signups)

# Pull PN for Untangle Signups
untangle_signups_pn <- glue_sql("SELECT phoenix.northstar_id,
                      phoenix.event_name,
                      CASE WHEN phoenix.event_name = 'converted on quiz' THEN 1 ELSE 0 END as completed
                      FROM public.phoenix_events phoenix
                      WHERE phoenix.campaign_id='9003'
                      AND phoenix.northstar_id in ({nsids*})
                      GROUP BY 1,2",
                       .con=pg,
                       nsids=untangle_signups$northstar_id)

untangle_signups_pn <- runQuery(untangle_signups_pn)

#Pull Quiz data
untangle_quiz <- ("SELECT 
                    e.records #>> '{user,northstarId}' AS northstar_id,
                    to_timestamp((e.records #>> '{meta,timestamp}')::bigint/1000) AS meta_timestamp,
                    e.records #>> '{page,path}' AS url,
                    e.records #> '{data,responses}' ->> '0' AS response_Q1,
                    e.records #> '{data,responses}' ->> '1' AS response_Q2,
                    e.records #> '{data,responses}' ->> '2' AS response_Q3,
                    e.records #> '{data,responses}' ->> '3' AS response_Q4,
                    e.records #> '{data,responses}' ->> '4' AS response_Q5,
                    e.records #> '{data,responses}' ->> '5' AS response_Q6,
                    phoenix.event_name,
                    CASE WHEN phoenix.event_name = 'converted on quiz' THEN 1 ELSE 0 END as completed,
                    CASE WHEN phoenix.event_name = 'phoenix_abandoned_quiz' THEN 1 ELSE 0 END as incomplete,
                    max(to_timestamp(phoenix.ts/1000)) AS ending_ts
                  FROM puck.events_json e
                  INNER JOIN public.phoenix_events phoenix ON phoenix.northstar_id=e.records #>> '{user,northstarId}'
                  WHERE e.records #>> '{data,responses}' IS NOT NULL 
                        AND e.records #>> '{page,path}' ILIKE '%untangle-the-web-quiz%'
                        AND e.records #>> '{page,path}' NOT ILIKE'%mock%'
                  GROUP BY 1,2,3,4,5,6,7,8,9,10,11")

untangle_quiz<- runQuery(untangle_quiz)

#Create vars and only look at quiz answer (exclude Tej's test cases that hapenned pre-launch date)
untangle_quiz <- untangle_quiz%>%
  mutate(meta_timestamp=as.Date(meta_timestamp))%>%
  filter(meta_timestamp>='2019-03-01')%>%
  mutate(
    q1_distract_hw = 
      case_when(response_q1==0 ~ 'Shopping for deals',
                response_q1==1 ~ 'Breaking news',
                response_q1==2 ~ 'Spreading the word about an important cause',
                response_q1==3 ~ 'Sending lol-worthy memes to friends',
                response_q1==4 ~ 'Making sure no one has hacked into your insta'),
    q2_meme = 
      case_when(response_q2==0~'Wheezing Spongebob',
                response_q2==1 ~ 'Squinting Woman',
                response_q2==2 ~ 'Change My Mind',
                response_q2==3 ~ 'Suprised Pikachu',
                response_q2==4 ~ 'Young Cardi',
                response_q2==5 ~ 'Gym Kardashian'),
    q3_mad_online = 
      case_when(response_q3==0 ~ 'Spammers, scams, and fraud',
                response_q3==1 ~ 'When people spread BS info',
                response_q3==2 ~ 'Injustice towards marginalized groups',
                response_q3==3 ~ 'Trolls and negativity',
                response_q3==4 ~ 'When brands bite each others styles'),
    q4_youtube_bff = 
      case_when(response_q4==0~'Emma Chamberlain',
                response_q4==1~'Superwoman',
                response_q4==2~'David Dobrik',
                response_q4==3~'Liza Koshy',
                response_q4==4~'The Dolan Twins',
                response_q4==5~'Jackie Aina'),
    q5_wakeup = 
      case_when(response_q5==0~'The news',
                response_q5==1 ~ 'The latest post from a meme Instagram account',
                response_q5==2 ~ 'The newest release from your favorite band',
                response_q5==3 ~ 'Tweets from activits you follow',
                response_q5==4 ~ 'Your bank account'),
    q6_afraid = 
      case_when(response_q6==0~'Accidentally liking your exes Instagram',
                response_q6==1 ~ 'Someone resurfacing an old embarrassing photo of you',
                response_q6==2 ~ 'Getting blocked by your crush',
                response_q6==3 ~ 'Dropping your phone in the toilet',
                response_q6==4 ~ 'Running out of battery while listening to music')
  )


#Only look at those who completed entire survey, select the lastest entry for each northstar, remove duplicate northstar ids
untangle_completed <- untangle_quiz%>%
  group_by(northstar_id)%>%
  filter(ending_ts==max(ending_ts))%>%
  ungroup(norhtstar_id)

#Question 1 - 
count(untangle_completed,q1_distract_hw,sort=TRUE)%>%
  filter(!is.na(q1_distract_hw))%>%
  mutate(pct=percent(n/sum(n)))%>%
  print()

#Question 2 - 
count(untangle_completed,q2_meme, sort=TRUE)%>%
  filter(!is.na(q2_meme))%>%
  mutate(pct=percent(n/sum(n)))%>%
  print()

#Question 3 - 
count(untangle_completed,q3_mad_online, sort=TRUE)%>%
  filter(!is.na(q3_mad_online))%>%
  mutate(pct=percent(n/sum(n)))%>%
  print()

#Question 4 - 
count(untangle_completed,q4_youtube_bff, sort=TRUE)%>%
  filter(!is.na(q4_youtube_bff))%>%
  mutate(pct=percent(n/sum(n)))%>%
  print()

#Question 5 - 
count(untangle_completed,q5_wakeup, sort=TRUE)%>%
  filter(!is.na(q5_wakeup))%>%
  mutate(pct=percent(n/sum(n)))%>%
  print()

#Question 6 - 
count(untangle_completed,q6_afraid, sort=TRUE)%>%
  filter(!is.na(q6_afraid))%>%
  mutate(pct=percent(n/sum(n)))%>%
  print()

##################################################################
###################### GUIDES ###################################
##################################################################

#Pull Phoenix Next Data to look at counts of views for each guide
untangled_guide_types <- sql("SELECT 
                   COUNT(DISTINCT (CASE WHEN pe.href LIKE '%untangle-the-web/secret-agent%'
                   then pe.event_id else NULL end)) AS secret_agent,
                   COUNT(DISTINCT (case when pe.href LIKE '%untangle-the-web/style-icon%'
                   then pe.event_id else NULL end)) AS style_icon,
                   COUNT(DISTINCT (case when pe.href LIKE '%untangle-the-web/news-detective%'
                   then pe.event_id else NULL end)) AS news_detective,
                   COUNT(DISTINCT (case when pe.href LIKE '%untangle-the-web/social-change-champion%'
                   then pe.event_id else NULL end)) AS social_change_champion,
                   COUNT(DISTINCT (case when pe.href LIKE '%untangle-the-web/ray-of-sunshine%'
                   then pe.event_id else NULL end)) AS ray_sunshine
                   FROM public.phoenix_events pe
                   WHERE pe.event_name = 'view'")

untangled_guide_types <-runQuery(untangled_guide_types)

#reshape from wide to long
guide_total <-melt(untangled_guide_types)


guide_total <- guide_total%>%
  rename(guide=variable)%>%
  mutate(total=as.numeric(sum(value)),
         pct=(percent(value/total)),
         guide=
           case_when(
             guide=='secret_agent' ~ 'Secret Agent',
             guide=='style_icon' ~ 'Style Icon',
             guide=='news_detective' ~ 'News Detective',
             guide=='social_change_champion' ~ 'Social Change Champion',
             guide=='impatient' ~ 'Impatient Player',
             guide=='ray_sunshine' ~ 'Ray of Sunshine'))

print(guide_total)

