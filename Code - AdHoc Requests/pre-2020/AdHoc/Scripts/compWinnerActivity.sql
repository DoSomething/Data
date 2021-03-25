SELECT 
	ca.northstar_id as 'northstar_id',
	u.email,
	ca.campaign_run_id as 'campaign_run_id',
	ca.signup_created_at,
	CASE WHEN gu.user_id IS NOT NULL THEN 1 ELSE 0 END AS competitor,
    max(CASE WHEN ca.post_id <> -1 THEN 1 ELSE 0 END) AS reportedback
FROM quasar.campaign_activity ca
LEFT JOIN quasar.users u ON ca.northstar_id = u.northstar_id
LEFT JOIN gladiator.users gu
	ON ca.northstar_id = gu.user_id AND ca.campaign_id = gu.campaign_id 
LEFT JOIN gladiator.contest gc
	ON gu.contest_id = gc.id AND ca.campaign_run_id = gc.campaign_run_id
WHERE ca.signup_created_at >= '2017-01-01'
GROUP BY ca.signup_id;

