--https://trello.com/c/AQ7A7SZs/1182-data-request-quality-of-defend-dreamers-participants
SELECT 
	list.event_name,
	list.dd_flag,
	count(DISTINCT list.nsid)
FROM 
	(SELECT 
		e.event_id,
		e.northstarid_s AS nsid,
		e.event_name,
		CASE WHEN e.campaign_id = '7927' THEN 'defend_dreamers' ELSE 'non_defend_dreamers' END AS dd_flag
	FROM phoenix_next_events e 
	WHERE e.northstarid_s IS NOT NULL 
	AND e.event_name IN ('view','clicked facebook share')) list
GROUP BY list.event_name, list.dd_flag
ORDER BY list.dd_flag