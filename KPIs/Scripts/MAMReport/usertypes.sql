SELECT 
	actions.action_month::DATE AS "Month",
	actions.niche,
	actions."Type",
	count(DISTINCT actions.northstar_id) AS nsids
FROM 
	(SELECT 
		m.northstar_id,
		CASE WHEN u.niche = 'Niche' THEN 'Niche' ELSE 'Non-Niche' END AS niche,
		CASE WHEN date_trunc('month', m."timestamp") = u.created_month THEN 'New' ELSE 'Returning' END AS "Type",
		date_trunc('month', m."timestamp") AS action_month
	FROM member_event_log m
	LEFT JOIN 
		(SELECT DISTINCT 
			use.id AS northstar_id, 
			use.created_at,
			use."source",
			date_trunc('month', use.created_at) AS created_month,
			CASE WHEN use."source" = 'niche' THEN 'Niche' ELSE 'Non-Niche' END AS niche
		FROM northstar.users use) u ON m.northstar_id = u.northstar_id
	WHERE m."source" IS DISTINCT FROM 'niche'
	AND u."source" IS DISTINCT FROM 'quasar-etl-node') actions
WHERE actions.action_month >= '2017-01-01'
GROUP BY actions.action_month, actions.niche, actions."Type"
;