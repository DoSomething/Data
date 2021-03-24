SELECT 
	temp.uid AS 'drupal_id',
	temp.campaign_run_id AS 'run_id',
	temp.signup_source AS 'signup_source',
	temp.signup_created_at AS 'signup_created_at_timestamp'
FROM 
	(SELECT DISTINCT 
		mml.mu_uid AS 'uid',
		mml.mc_run_id AS 'campaign_run_id',
		'sms' AS 'signup_source',
		min(mml.ms_activated_at) AS 'signup_created_at'
	FROM 
		users_and_activities.mobile_master_lookup_lite mml
	LEFT JOIN 
		dosomething.dosomething_signup ds
		ON mml.mu_uid = ds.uid AND mml.mc_run_id = ds.run_nid
	WHERE 
		ds.uid IS NULL
		AND ds.nid IS NULL
		AND ds.run_nid IS NULL
		AND mml.mu_uid IS NOT NULL
		AND mml.mc_run_id IS NOT NULL
	GROUP BY mml.mu_uid, mml.mc_run_id) temp