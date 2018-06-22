source('config/init.R')
source('config/pgConnect.R')
library(glue)
pg <- pgConnect()

qres <- runQuery(
  "SELECT 
    m.northstar_id,
    EXTRACT('week' FROM m.\"timestamp\") AS action_week,
    m.action_type,
    count(*) AS action_count
  FROM public.member_event_log m 
  WHERE m.\"timestamp\" >= '2018-01-01'
  GROUP BY m.northstar_id, EXTRACT('week' FROM m.\"timestamp\"), m.action_type"
  )

col <- 
  qres %>% 
  mutate(
    action = 
      case_when(
        action_type %in% c('signup','facebook share completed','post','registered') ~ 'Campaigns',
        action_type %in% c('messaged_gambit','sms_link_click') ~ 'SMS',
        action_type %in% c('site_access','site_login') ~ 'Web',
        action_type %in% c('clicked_link') ~ 'Email',
        action_type %in% 'account_creation' ~ 'Account Creation',
        TRUE ~ ''
      )
  ) %>% 
  group_by(action_week, action) %>%
  summarise(
    actions = sum(action_count)
  )

ggplot(col, aes(x=action_week,y=actions)) + 
  geom_smooth(se=F, aes(color=action)) + 
  #geom_bar(stat='identity',aes(fill=action),alpha=.2) +
  geom_line(aes(color=action), size=.4) +
  scale_fill_brewer(palette = 'Set2') +
  scale_color_brewer(palette = 'Set2') +
  stat_summary(fun.y = sum, group = 3, geom = "bar", alpha=.2)
