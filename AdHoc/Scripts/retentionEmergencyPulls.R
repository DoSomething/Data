source('config/init.R')

# 2: repeat RB rate (multiple RBUs/all RBUs) for the last 365, 180, 90,60,30. 

rbs <- 
  runQuery(
    "SELECT 
      northstar_id,
      post_id, 
      post_created_at
    FROM reportbacks r
    WHERE r.post_created_at >= now() - INTERVAL '365 days'
      AND r.post_type <> 'voter-reg'"
    )

mutSet <- 
  rbs %>% 
  mutate(
    flag180 = ifelse(post_created_at >= today()-180, 1, 0),
    flag90 = ifelse(post_created_at >= today()-90, 1, 0),
    flag60 = ifelse(post_created_at >= today()-60, 1, 0),
    flag30 = ifelse(post_created_at >= today()-30, 1, 0),
  ) %>% 
  group_by(northstar_id) %>% 
  summarise(
    multi.365 = ifelse(n()>1,1,0),
    multi.180 = ifelse(sum(flag180)>1,1,0),
    multi.90 = ifelse(sum(flag90)>1,1,0),
    multi.60 = ifelse(sum(flag60)>1,1,0),
    multi.30 = ifelse(sum(flag30)>1,1,0)
  ) 

out <- 
  mutSet %>% 
  summarise(
    multiRate.365 = mean(multi.365),
    multiRate.180 = mean(multi.180),
    multiRate.90 = mean(multi.90),
    multiRate.60 = mean(multi.60),
    multiRate.30 = mean(multi.30),
  ) %>% 
  pivot_longer(cols=everything(), names_to = 'multipleRBRate',values_to = 'pct') %>% 
  mutate(
    multipleRBRate = gsub('multiRate.','',multipleRBRate),
    pct = round(pct*100,1)
  ) 

# 3. Days elapsed since last action for RBU365

qres <- 
  runQuery(
    "SELECT DISTINCT
      mel.timestamp, 
      mel.action_type,
      mel.northstar_id
    FROM reportbacks r
    LEFT JOIN member_event_log mel 
      ON r.northstar_id=mel.northstar_id 
      AND mel.timestamp >= r.post_created_at
    WHERE r.post_type <> 'voter-reg'
      AND r.post_created_at >= now() - INTERVAL '1 year'"
  )

rbAct <- 
  qres %>% 
  group_by(northstar_id) %>% 
  arrange(northstar_id, timestamp) %>% 
  mutate(
    prev_action = lag(timestamp),
    timeToSubsequentAction = difftime(timestamp,prev_action, units='days'),
    actionCount = n()
  )

rbAct %>% 
  ungroup() %>% 
  summarise(
    pctNoFurtherActions = length(which(actionCount==1)) / length(unique(northstar_id)),
    medianMoreActions = median(actionCount),
    avgTimeToNextAction = as.numeric(mean(timeToSubsequentAction, na.rm=T))
  ) %>% 
  pivot_longer(
    cols=everything(),
    names_to = 'Metric',
    values_to = 'Value'
    )

