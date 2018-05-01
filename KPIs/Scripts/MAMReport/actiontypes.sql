SELECT 
	actions.action_month::DATE AS "Month",
	actions.niche,
	actions.action_type,
	count(*) AS actions
FROM 
	(SELECT 
		m.northstar_id,
		u.niche,
		m.action_type,
		date_trunc('month', m."timestamp") AS action_month
	FROM 
		(SELECT DISTINCT 
			use.id AS northstar_id, 
			CASE WHEN use."source" = 'niche' THEN 'Niche' ELSE 'Non-Niche' END AS niche
		FROM northstar.users use
		WHERE use."source" IS DISTINCT FROM 'quasar-etl-node'
		AND use.created_at >= '2017-01-01') u
	LEFT JOIN member_event_log m ON m.northstar_id = u.northstar_id
	WHERE m."timestamp" >= '2017-01-01') actions 
WHERE actions.action_month >= '2017-01-01'
GROUP BY actions.action_month, actions.niche, actions.action_type
;
