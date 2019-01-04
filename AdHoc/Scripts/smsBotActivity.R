# https://trello.com/c/YrHZpAbn/1409-bots-sms

source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

q <- 
  "SELECT
    northstar_id,
    SUM(
      CASE WHEN action_type = 'bertly_link_click' THEN 1 ELSE 0 END
    ) AS link_clicks,
    SUM(
      CASE WHEN action_type = 'messaged_gambit' THEN 1 ELSE 0 END
    ) AS sms_messages,
  count(*) AS total_sms_actions
  FROM member_event_log
  WHERE timestamp > '2018-06-01'
  AND action_type IN ('bertly_link_click','messaged_gambit')
  GROUP BY northstar_id"

qres <- runQuery(q)

dat <- 
  qres %>% 
  mutate(
    click_percentile = percent_rank(link_clicks),
    message_percentile = percent_rank(sms_messages),
    click_rank = min_rank(link_clicks),
    message_rank = min_rank(sms_messages),
    action_percentile = percent_rank(total_sms_actions),
    action_rank = min_rank(total_sms_actions)
  )

outlie <- 
  dat %>% 
  filter(action_percentile > .9999)

ggplot(
  outlie, 
  aes(x=reorder(northstar_id, -total_sms_actions),y=total_sms_actions)
  ) + 
  geom_point() + geom_line(aes(group=1), linetype='dotted') + 
  labs(x='') + theme(axis.text.x = element_text()) +
  scale_y_continuous(breaks=pretty_breaks(20)) +
  scale_x_discrete(labels=as.character(seq(1,42,1)))

