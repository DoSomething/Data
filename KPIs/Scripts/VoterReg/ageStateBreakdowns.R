# https://trello.com/c/6FtWrlET/1353-voter-reg-geographic-breakdown
# Step 1, run rockTheVote.R file but grab age and state

stateBreakdown <-
  rtv %>%
  filter(grepl('register', ds_vr_status)) %>%
  count(home_state) %>%
  mutate(p=n/sum(n)) %>%
  select(-n) %>%
  left_join(tibble(state.name, home_state=state.abb))

ageBreakdown <-
  rtv %>%
  filter(!is.na(date_of_birth)) %>%
  select(date_of_birth) %>%
  mutate(age=as.Date(date_of_birth, '%m/%d/%Y')) %>%
  filter(age<today()) %>%
  mutate(age=floor(age_calc(age, units='years'))) %>%
  filter(age<100 & age>=18) %>% count(age) %>%
  mutate(pct=n/sum(n))

ggplot(ageBreakdown, aes(x=age, y=pct)) +
  geom_bar(stat='identity',fill='#6ac6b4',alpha=.8,width = .8) +
  geom_text(aes(label=percent(pct)), size=2, hjust=-.5) +
  scale_x_continuous(breaks=pretty_breaks(40)) +
  labs(title='Age Breakdown', x='Age',y='Percent of Total') +
  coord_flip() +
  theme(plot.title=element_text(hjust=.5))