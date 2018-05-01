SELECT 
	actions.action_month,
	actions.niche,
	actions."new",
	count(DISTINCT actions.northstar_id) AS nsids
FROM 
	(SELECT 
		m.northstar_id,
		u.niche,
		CASE WHEN date_trunc('month', m."timestamp") = u.created_month THEN 1 ELSE 0 END AS "new",
		date_trunc('month', m."timestamp") AS action_month
	FROM 
		(SELECT DISTINCT 
			use.id AS northstar_id, 
			use.created_at,
			date_trunc('month', use.created_at) AS created_month,
			CASE WHEN use."source" = 'niche' THEN 1 ELSE 0 END AS niche
		FROM northstar.users use
		WHERE use."source" IS DISTINCT FROM 'quasar-etl-node') u
	LEFT JOIN member_event_log m ON m.northstar_id = u.northstar_id) actions
WHERE actions.action_month >= '2017-01-01'
GROUP BY actions.action_month, actions.niche, actions."new"
;