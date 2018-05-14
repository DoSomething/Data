SELECT 
	date_trunc('day', m."timestamp")::date AS "date",
	count(DISTINCT m.northstar_id) AS active_members
FROM member_event_log m
WHERE m."timestamp" >= '2017-01-01'
AND m."source" IS DISTINCT FROM 'niche'
GROUP BY date_trunc('day', m."timestamp")
;
