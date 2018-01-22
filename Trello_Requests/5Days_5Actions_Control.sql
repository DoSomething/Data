SELECT 
	c.northstar_id,
	i.campaign_run_id,
	i.campaign_run_id_title,
	c.submission_created_at,
	u.first_name,
	u.last_name,
	u.email,
	u.mobile,
	u.source,
	c.signup_created_at,
	sum(case when post_id <> -1 AND c.submission_created_at >= '2017-08-01' AND c.submission_created_at <= '2017-08-31' then 1 else 0 end) as total_rbs_30,
	sum(case when post_id <> -1 AND c.submission_created_at > '2017-08-31' AND c.submission_created_at <= '2017-09-30' then 1 else 0 end) as total_rbs_60,
	sum(case when post_id <> -1 AND c.submission_created_at > '2017-09-30' AND c.submission_created_at < '2017-10-31' then 1 else 0 end) as total_rbs_90
	FROM quasar.campaign_activity c
LEFT JOIN quasar.users u ON c.northstar_id=u.northstar_id
LEFT JOIN quasar.campaign_info i ON i.campaign_run_id = c.campaign_run_id 
WHERE u.northstar_id IN ('57cf6cf342a06413068b4592',
GROUP BY c.northstar_id