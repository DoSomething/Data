source('config/init.R')

q <- runQuery(
  "WITH elist AS  
	(SELECT 
		e.session_id,
		s.landing_datetime,
		s.end_datetime,
		e.event_name,
		e.event_datetime,
	     count(*) OVER (PARTITION BY e.session_id) AS events_in_session
	FROM phoenix_events e
	INNER JOIN phoenix_sessions s ON e.session_id = s.session_id
	WHERE e.event_datetime >= '2019-01-01'
	AND e.page_utm_source NOT ILIKE '%snapchat%'
	AND e.event_name NOT IN ('visit','waypoint reached')
	)
  SELECT 
  	elist.session_id,
  	max(elist.events_in_session) as events_in_session,
  	max(elist.end_datetime) as end_ts, 
  	max(elist.landing_datetime) as start_ts
  FROM elist
  GROUP BY elist.session_id"
  ) %>% 
  mutate(
    singleEventSession = ifelse(events_in_session>1,F,T),
    sessionLength = end_ts-start_ts
  ) %>% 
  filter(sessionLength>0)

ntileDat <- 
  q %>% 
  group_by(singleEventSession) %>% 
  summarise(
    ntile = list(enframe(quantile(sessionLength, probs=seq(0,1,.05)))),
    groupTotal=n(),
    grandTotal=nrow(.)
  ) %>% 
  unnest %>% 
  mutate(
    popCount = groupTotal*as.numeric(gsub('%','',name))/100,
    popPct = popCount/grandTotal
  ) %>% 
  rename(
    ntile = name,
    sessionLength = value
  )

  
ggplot(q %>% filter(sessionLength<1500), aes(x=sessionLength)) + 
  geom_density() +
  facet_wrap(~singleEventSession)

saveCSV(ntileDat)
