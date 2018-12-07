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
