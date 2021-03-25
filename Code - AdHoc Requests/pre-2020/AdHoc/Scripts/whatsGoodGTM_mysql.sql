SELECT 
	gtm.northstar_id,
	gtm.did_gtm,
	camp.signups, 
	camp.posts,
	actions.site_visits,
	actions.total_actions,
	actions.active_jan,
	actions.actions_jan,
	actions.active_feb,
	actions.actions_feb,
	actions.active_march,
	actions.actions_march
FROM 
	(SELECT 
		c.northstar_id,
		max(CASE WHEN c.campaign_run_id = 8022 THEN 1 ELSE 0 END) AS did_gtm
	FROM campaign_activity c
	WHERE c.signup_created_at >= '2018-01-01' AND c.signup_created_at < '2018-04-01'
	GROUP BY c.northstar_id) gtm
LEFT JOIN 
	(SELECT 
		c1.northstar_id,
		count(DISTINCT c1.signup_id) AS signups,
		sum(CASE WHEN c1.post_id <> -1 THEN 1 ELSE 0 END) AS posts
	FROM campaign_activity c1
	WHERE c1.signup_created_at >= '2018-01-01' AND c1.signup_created_at < '2018-04-01'
	GROUP BY c1.northstar_id
	) camp ON gtm.northstar_id = camp.northstar_id
LEFT JOIN 
	(SELECT
		mel.northstar_id,
		max(CASE WHEN mel.`timestamp` >= '2018-01-01' AND mel.`timestamp` < '2018-02-01' THEN 1 ELSE 0 END) AS active_jan,
		sum(CASE WHEN mel.`timestamp` >= '2018-01-01' AND mel.`timestamp` < '2018-02-01' THEN 1 ELSE 0 END) AS actions_jan,
		max(CASE WHEN mel.`timestamp` >= '2018-02-01' AND mel.`timestamp` < '2018-03-01' THEN 1 ELSE 0 END) AS active_feb,
		sum(CASE WHEN mel.`timestamp` >= '2018-02-01' AND mel.`timestamp` < '2018-03-01' THEN 1 ELSE 0 END) AS actions_feb,
		max(CASE WHEN mel.`timestamp` >= '2018-03-01' AND mel.`timestamp` < '2018-04-01' THEN 1 ELSE 0 END) AS active_march,
		sum(CASE WHEN mel.`timestamp` >= '2018-03-01' AND mel.`timestamp` < '2018-04-01' THEN 1 ELSE 0 END) AS actions_march,
		sum(CASE WHEN mel.action_type IN ('site_access','site_login') THEN 1 ELSE 0 END) AS site_visits,
		count(*) AS total_actions
	FROM member_event_log mel
	WHERE mel.`timestamp` >= '2018-01-01' AND mel.`timestamp` < '2018-04-01'
	GROUP BY mel.northstar_id) actions ON gtm.northstar_id = actions.northstar_id
	;
