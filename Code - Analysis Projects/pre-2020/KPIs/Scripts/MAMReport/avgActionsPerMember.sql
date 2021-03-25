SELECT 
	ns_actions."Month"::DATE AS "Month",
	ns_actions."Type",
	ns_actions.niche,
	avg(ns_actions.actions) AS avg_n_actions
FROM 
	(SELECT 
		actions.northstar_id, 
		actions.action_month::DATE AS "Month",
		actions."Type",
		actions.niche,
		count(*) AS actions
	FROM 
		(SELECT 
			m.northstar_id,
			u.niche,
			CASE WHEN date_trunc('month', m."timestamp") = u.created_month THEN 'New' ELSE 'Returning' END AS "Type",
			date_trunc('month', m."timestamp") AS action_month
		FROM 
			(SELECT DISTINCT 
				use.id AS northstar_id, 
				CASE WHEN use."source" = 'niche' THEN 'Niche' ELSE 'Non-Niche' END AS niche,
				date_trunc('month', use.created_at) AS created_month
			FROM northstar.users use
			WHERE use."source" IS DISTINCT FROM 'quasar-etl-node'
			AND use.created_at >= '2017-01-01') u
		LEFT JOIN member_event_log m ON m.northstar_id = u.northstar_id
		WHERE m."timestamp" >= '2017-01-01'
		AND m."source" IS DISTINCT FROM 'niche') actions 
	WHERE actions.action_month >= '2017-01-01'
	GROUP BY actions.northstar_id, actions.action_month, actions.niche, actions."Type") ns_actions
GROUP BY ns_actions."Month"::DATE, ns_actions.niche, ns_actions."Type"
;