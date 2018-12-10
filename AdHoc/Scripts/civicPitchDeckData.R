load("~/Data/SPIS/spisEnv.RData")

# Comment in pitch deck ---------------------------------------------------

ggplot(planVote, aes(x=plan_to_vote_if_eligible,y=count, fill=Group)) +
  geom_bar(stat='identity',width = .8, position='dodge') +
  geom_text(aes(x=plan_to_vote_if_eligible,y=count,label=round(count)),vjust=-.1,size=3.4, position=position_dodge(width=.8)) +
  labs(title='Do You Plan to Vote', x='',y='') +
  theme(plot.title=element_text(hjust=.5))


ggplot(planvoteElig, aes(x=vote_when_eligible,y=count, fill=Group)) +
  geom_bar(stat='identity',width = .8, position='dodge') +
  geom_text(aes(x=vote_when_eligible,y=count,label=round(count)),vjust=-.1,size=3.4, position=position_dodge(width=.8)) +
  labs(title='Do You Plan to Vote When You Are Eligible', x='',y='') +
  theme(plot.title=element_text(hjust=.5))


# Reportbacks -------------------------------------------------------------

toPlot <- 
  yoy %>% 
  filter(
    (grepl('12-31', date) | yearRunningTotal==max(yearRunningTotal,na.rm=T)) &
      year < 2019
  )

ggplot(toPlot, aes(x=year,y=yearRunningTotal)) +
  geom_bar(stat='identity',width = .8,fill='#6ac6b4') +
  geom_text(aes(label=yearRunningTotal),vjust=-.1,size=3.4) +
  labs(title='Verified Social Change Actions', x='',y='') +
  theme(plot.title=element_text(hjust=.5))

# Voter registrations by channel ------------------------------------------
library(tools)

vrSource <- 
  vr %>%
  group_by(source) %>%
  summarise(
    tot_vot_reg = grepl('register', ds_vr_status) %>% sum()
  ) %>% 
  mutate(
    Source = gsub('_', ' ', source),
    Source = toTitleCase(Source)
  )

ggplot(vrSource, aes(x=Source,y=tot_vot_reg)) +
  geom_bar(stat='identity',width = .8,fill='#6ac6b4') +
  geom_text(aes(label=tot_vot_reg),vjust=-.1,size=3.4) +
  labs(title='Voter Registrations by Channel', x='',y='') +
  theme(plot.title=element_text(hjust=.5))

# Age of registrants ------------------------------------------------------
library(eeptools)
q <- 
  glue_sql(
    "SELECT
      u.northstar_id AS nsid,
      u.birthdate
    FROM public.users u
    WHERE u.northstar_id IN ({nsids*})
    ",
    .con = pg,
    nsids = unique(vr %>% filter(grepl('register',ds_vr_status)) %$% nsid)
  )

dobs <- 
  runQuery(q)

toPlot <- 
  dobs %>% 
  filter(!is.na(birthdate)) %>% 
  mutate(
    age = round(age_calc(as.Date(birthdate), units='years'))
  ) %>% 
  count(age) %>% 
  filter(age>=18 & age < 50) %>% 
  mutate(pct = n/sum(n))

ggplot(toPlot, aes(x=age,y=pct)) +
  geom_bar(stat='identity',width = .8,fill='#6ac6b4') +
  geom_text(aes(label=percent(pct)),vjust=-.1,size=2) +
  labs(title='Voter Registrant Age Distribution', x='',y='') +
  theme(plot.title=element_text(hjust=.5)) + 
  scale_x_continuous(breaks=seq(18,50,1))


# Reportback Locations ----------------------------------------------------

q <-
  "
SELECT
  UPPER(SUBSTRING(dist.state,1,2)) AS state,
  sum(dist.rbs) as reportbacks
FROM
  (SELECT DISTINCT
    c.northstar_id,
    u.state,
    COALESCE(c.campaign_run_id::VARCHAR, c.campaign_id) as campaign_id,
    c.signup_id,
    c.post_class,
    c.reportback_volume AS rbs,
    c.post_attribution_date::date AS date
  FROM public.campaign_activity c
  INNER JOIN public.users u ON u.northstar_id = c.northstar_id
  WHERE c.post_attribution_date IS NOT NULL
  AND c.post_attribution_date >= '2018-01-01'
  AND c.post_status IN
  ('accepted','pending','register-OVR','register-form','confirmed')) dist
GROUP BY UPPER(SUBSTRING(dist.state,1,2))
"
qres <- runQuery(q)

stateBreak <- 
  qres %>% 
  filter(!is.na(state) & reportbacks > 100 & nchar(state)==2) %>% 
  mutate(pct = reportbacks / sum(reportbacks))

ggplot(stateBreak, aes(x=state,y=pct)) +
  geom_bar(stat='identity',width = .8,fill='#6ac6b4') +
  geom_text(aes(label=percent(pct)),vjust=-.1,size=2) +
  labs(title='Reportbacks by State', x='',y='') +
  theme(plot.title=element_text(hjust=.5))


# Voter Registrations by State --------------------------------------------

regNSIDs <- 
  vr %>% 
  filter(grepl('register',ds_vr_status))%$% 
  nsid

q <- 
  glue_sql(
    "SELECT 
      UPPER(SUBSTRING(u.state,1,2)) AS state,
      count(*)
    FROM public.users u
    WHERE 
      u.northstar_id IN ({nsids*}) AND
      u.state IS NOT NULL
    GROUP BY UPPER(SUBSTRING(u.state,1,2))",
    .con=pg,
    nsids=regNSIDs
  )

qres <- runQuery(q)

vrState <- 
  qres %>% 
  filter(count>10 & nchar(state)==2) %>% 
  mutate(pct = count / sum(count))

ggplot(vrState, aes(x=state,y=pct)) +
  geom_bar(stat='identity',width = .8,fill='#6ac6b4') +
  geom_text(aes(label=percent(pct)),vjust=-.1,size=2) +
  labs(title='Voter Registrations by State', x='',y='') +
  theme(plot.title=element_text(hjust=.5))


# I Voted Reportbacks -----------------------------------------------------

q <-
  "
SELECT
  UPPER(SUBSTRING(dist.state,1,2)) AS state,
  sum(dist.rbs) as reportbacks
FROM
  (SELECT DISTINCT
    c.northstar_id,
    u.state,
    COALESCE(c.campaign_run_id::VARCHAR, c.campaign_id) as campaign_id,
    c.signup_id,
    c.post_class,
    c.reportback_volume AS rbs,
    c.post_attribution_date::date AS date
  FROM public.campaign_activity c
  INNER JOIN public.users u ON u.northstar_id = c.northstar_id
  WHERE c.post_attribution_date IS NOT NULL
  AND c.post_attribution_date >= '2018-01-01'
  AND c.post_status IN
  ('accepted','pending','register-OVR','register-form','confirmed')
  AND c.campaign_id = '7077') dist
GROUP BY UPPER(SUBSTRING(dist.state,1,2))
"
qres <- runQuery(q)

iVotedState <- 
  qres %>% 
  filter(!is.na(state) & nchar(state)==2) %>% 
  mutate(pct = reportbacks / sum(reportbacks))

ggplot(iVotedState, aes(x=state,y=pct)) +
  geom_bar(stat='identity',width = .8,fill='#6ac6b4') +
  geom_text(aes(label=percent(pct)),vjust=-.1,size=2) +
  labs(title='I Voted Reportbacks by State', x='',y='') +
  theme(plot.title=element_text(hjust=.5))
  

# Agreement Politics ------------------------------------------------------

whichIssues <- 
  c('Gun Violence','Immigration','The Environment',
    'Racial Equality','Sexual Harrassment/Assault')

ggplot(filter(causeImport.Politics, Group=='Gen Pop' & quest %in% whichIssues),
       aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  labs(title='How Important are Each of These Causes?',
       x='Level of Importance',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels = c('Not at all important','Not Important','Neutral',
                                'Somewhat Important','Very Important')) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))

whichPositions <- 
  c('Background Checks are Needed for All Guns',
    'Climate Change is Man Made',
    'Sexual Harassment is a Sign of Societal Issues',
    'Government Should Provide All Healthcare',
    'Undocumented Immigrants Should Have Path to Citizenship'
    )

ggplot(filter(agreePosition.Politics, Group=='Gen Pop' & quest %in% whichPositions),
       aes(x=avgVal, y=reorder(quest,-meanVal))) +
  geom_point(aes(colour=political_view), size=2) +
  facet_wrap(~Group) +
  labs(title='How Much Do You Agree With the Following Positions',
       x='',y='') +
  scale_colour_brewer(palette = 'RdYlBu') +
  scale_x_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
  guides(colour=guide_legend(title="Political Views")) +
  theme(plot.title=element_text(hjust=.5),
        axis.text.x = element_text(angle=30,hjust=1))
