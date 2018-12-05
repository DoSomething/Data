pg<- pgConnect()

#upload NFL Member survey data, remove dups and only look at completed
nfl_members <- read.csv('~/Documents/NFL/NFL Survey Members.csv')%>%
  rename(nsid=External.Reference)%>%
  mutate(nsid=as.character(nsid))%>%
  filter(Response.Status=='Completed' & !duplicated(nsid))

#Pull members who didn't do NFL survey to compare against, pull activity data for both groups (signups, actions,rbs)
nfl_activity <- glue_sql("SELECT email.*,activity.total_signups,activity.posts,mam.active_lastmonth, mam.count_actions
                          FROM 
                            (SELECT DISTINCT 
                              cio.customer_id AS nsid,
                              max(CASE WHEN cio.customer_id IN ({nsids*}) THEN 1 ELSE 0 END) AS nfl_survey
                            FROM public.email_event cio
                            WHERE (cio.event_type = 'email_opened' AND cio.timestamp >= '2018-05-27')  
                            GROUP BY 1) email
                          LEFT JOIN
                            (SELECT DISTINCT 
                              ca.northstar_id as nsid,
                              COUNT(DISTINCT ca.signup_id) AS total_signups,
                              sum(CASE WHEN ca.post_id <> -1 THEN 1 ELSE 0 END) AS posts
                            FROM public.campaign_activity ca 
                            GROUP BY ca.northstar_id) activity
                            ON email.nsid=activity.nsid
                          INNER JOIN 
                            (SELECT DISTINCT 
                              mel.northstar_id AS nsid,
                              max(CASE WHEN mel.timestamp >= now() - INTERVAL '1 month' then 1 else 0 end) as active_lastmonth,
                              COUNT(*) as count_actions
                            FROM public.member_event_log mel
                            GROUP BY mel.northstar_id) mam 
                          ON activity.nsid= mam.nsid",
                              nsids = nfl_members$nsid,
                              .con = pg)

nfl_activity<- runQuery(nfl_activity)

#Pull number of rbs per user
nfl_rbs <- glue_sql("SELECT 
                      email.nsid,
                      email.nfl_surveyflag,
                      sum(dist.rbs) as rbs
                    FROM 
                      (SELECT DISTINCT 
                        cio.customer_id AS nsid,
                        max(CASE WHEN cio.customer_id IN ({nsids*}) THEN 1 ELSE 0 END) AS nfl_surveyflag
                      FROM public.email_event cio
                      WHERE cio.event_type = 'email_opened' AND cio.timestamp >= '2018-05-28' 
                      GROUP BY 1) email
                    LEFT JOIN
                      (SELECT DISTINCT 
                        c.northstar_id as nsid,
                        c.signup_id,
                        COALESCE(c.campaign_run_id::VARCHAR, c.campaign_id) as campaign_id,
                        c.post_class,
                        c.reportback_volume AS rbs,
                        c.post_attribution_date::date AS date
                      FROM public.campaign_activity c
                      WHERE c.post_attribution_date IS NOT NULL 
                      AND c.post_attribution_date >= '2018-01-01'
                      AND c.post_status IN ('accepted','pending','register-OVR','register-form','confirmed')) dist
                    ON email.nsid=dist.nsid
                    GROUP BY email.nsid, email.nfl_surveyflag",
                    nsids = nfl_members$nsid,
                    .con = pg)

nfl_rbs <- runQuery(nfl_rbs)

#join survey repsonses with activity 
nfl_merged<- left_join(nfl_activity, nfl_members)

#join above with rbs
nfl_all<- left_join(nfl_merged, nfl_rbs)

#select relevant vars and recode missing counts to 0 for average scores
nfl_all <- nfl_all%>%
  select(nsid,nfl_survey,What.is.your.gender.,How.would.you.feel.towards.each.company.below.if.they.partnered.with.the.NFL...DoSomething.org,If.the.NFL.changed.its.policy.to.the.one.stated.in.the.previous.question..how.would.you.feel.towards.each.company.if.they.partnered.with.the.NFL.after.the.fact...DoSomething.org,total_signups,active_lastmonth,count_actions,rbs)%>%
  rename(like_DS=How.would.you.feel.towards.each.company.below.if.they.partnered.with.the.NFL...DoSomething.org)%>%
  mutate(reported_back=ifelse(rbs>0,1,0),
         count_actions=ifelse(is.na(count_actions),0,count_actions),
         rbs=ifelse(is.na(rbs),0,rbs),
         like_DS_cat=
           case_when(like_DS=='I would like them a little less' | like_DS=='I would like them a lot less' ~ 'Like DS less',
                     like_DS=='I would like them a little bit more' | like_DS=='I would like them a lot more' ~ 'Like DS more',
                     like_DS=='My feelings would stay the same' ~ 'Feelings would stay the same',
                     like_DS=='No opinion' ~ 'No opinion',
                     like_DS=="Don't know" ~ 'Dont know'))

#Average number of rbs for survey vs. conrtol
nfl_means <- nfl_all%>%
  group_by(nfl_survey)%>%
  summarise(mean_rbs = mean(rbs),
            mean_actions = mean(count_actions),
            mean_signups = mean(total_signups))

#Average rbs,signups,and actions per group by DS liking
nfl_means_likeDS <- nfl_all%>%
  group_by(nfl_survey,like_DS)%>%
  summarise(mean_rbs = mean(rbs),
            mean_actions = mean(count_actions),
            mean_signups = mean(total_signups))

#Average rbs,signups,and actions per group by DS liking (condensed cat)
nfl_means_likeDS <- nfl_all%>%
  group_by(nfl_survey,like_DS_cat)%>%
  summarise(mean_rbs = mean(rbs),
            mean_actions = mean(count_actions),
            mean_signups = mean(total_signups))

write.csv(nfl_means_likeDS, file = 'NFL survey sentiment.csv')

#bar charts
ggplot(nfl_all, aes(x=factor(like_DS_cat), y=total_signups)) + stat_summary(fun.y="mean", geom="bar") 
ggplot(nfl_all, aes(x=factor(like_DS_cat), y=count_actions)) + stat_summary(fun.y="mean", geom="bar")
ggplot(nfl_all, aes(x=factor(like_DS_cat), y=rbs)) + stat_summary(fun.y="mean", geom="bar")
