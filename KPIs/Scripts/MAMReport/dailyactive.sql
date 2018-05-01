SELECT 
	date_trunc('day', m."timestamp")::date AS "date",
	count(DISTINCT m.northstar_id) AS active_members
FROM member_event_log m
WHERE m."timestamp" >= '2017-01-01'
GROUP BY date_trunc('day', m."timestamp")
;
