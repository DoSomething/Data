source('config/init.R')
source('config/mySQLConfig.R')
library(broom)

###Winners
winners <- 
  read_csv('Data/competition_winners.csv') %>% 
  filter(!is.na(Email)) %>% 
  transmute(
    email = Email,
    competition = `Place/Competition Name`,
    enter_date = as.Date(`DATE ENTERED`, '%m/%d/%y'),
    win_date = as.Date(word(Status, -1),'%m/%d/%y')
  )

winnerLookup <- hash(winners$email, winners$enter_date)

###Gladiator
q <-
  "
SELECT 
  user_id as northstar_id,
  count(*) as n_competitions
FROM gladiator.users 
WHERE subscribed = 1 AND unsubscribed IS NULL
GROUP BY northstar_id
"

glad <- runQuery(q)

# winemails <- prepQueryObjects(winners$email)
# 
# winorths <- 
#   paste0(
#     "SELECT u.northstar_id, u.email
#     FROM quasar.users u 
#     WHERE u.email IN ",winemails,";"
#   )
# 
# winnorths <- runQuery(winorths)

###Member event log
read <- 
  read_csv('Data/mel_2017-10-02_2016-10-01.csv') %>% 
  setNames(c('email','northstar_id','action_type','event_id','ts','northstar_created')) %>% 
  mutate(event_id = seq(1,nrow(.), 1))

mel <-
  read %>% 
  tbl_dt() %>% 
  filter(action_type %in% c('reportback','sign-up','sms_game')) %>% 
  mutate(
    enter_date = as.Date(unlist(winners[match(email, winners$email),'enter_date']), '1970-01-01'),
    win_date = as.Date(unlist(winners[match(email, winners$email),'win_date']), '1970-01-01'),
    winner = if_else(email %in% winners$email, T, F)
  ) %>% 
  group_by(northstar_id) %>%
  filter(max(action_type=='sign-up')==1) %>%
  summarise(
    n_actions = n(),
    northstar_created = max(northstar_created),
    winner = max(winner),
    enter_date = max(enter_date),
    win_date = max(win_date),
    actions_pre_win = length(which(winner==T & ts < enter_date)),
    actions_post_win = length(which(winner==T & ts >= enter_date))
  ) %>% 
  left_join(glad) %>%
  mutate(
    n_competitions = if_else(is.na(n_competitions), 0, n_competitions),
    daysMember = as.numeric(Sys.Date() - northstar_created),
    daysMemberPreWin = as.numeric(enter_date - northstar_created),
    daysMemberPostWin = as.numeric(max(read$ts, na.rm=T)-enter_date)
  )

actionSum <-
  mel %>% 
  mutate(
    any_competitions = if_else(n_competitions > 0 | winner == T, T, F),
    avgActions.Pre = actions_pre_win / daysMemberPreWin,
    avgActions.Pre = if_else(avgActions.Pre==Inf, 0, avgActions.Pre),
    avgActions.Post = actions_post_win / daysMemberPostWin,
    avgActions.Post = if_else(avgActions.Post==Inf, 0, avgActions.Post)
    ) %>% 
  group_by(any_competitions, winner) %>%
  summarise(
    actionCount = mean(n_actions),
    actionsPreWin = mean(actions_pre_win),
    actionsPostWin = mean(actions_post_win),
    actionsPerDay.Pre = mean(avgActions.Pre, na.rm=T),
    actionsPerDay.Post = mean(avgActions.Post)
  )

actionMod <-
  lm(
    formula = n_actions ~ n_competitions + daysMember + winner,
    data = mel
  )

competitor <- 
  mel %>% 
  filter(n_competitions > 0)

winnerMod <-
  lm(
    formula = actions_post_competition ~ daysMember + actions_pre_win + n_competitions + winner,
    data = competitors
  )