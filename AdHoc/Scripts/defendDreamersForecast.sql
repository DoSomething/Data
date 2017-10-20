SELECT 
	count(DISTINCT c.signup_id) AS signups,
	sum(reportbacks.reportback) AS reportbacks,
	CASE WHEN c.campaign_run_id IN (7663, 7832, 7908) THEN 'Social Share'
		ELSE 'Card Making' END AS TYPE,
	c.campaign_run_id,
	info.campaign_run_start_date AS start_date,
	info.campaign_run_id_title campaign
FROM campaign_activity c
LEFT JOIN campaign_info info ON info.campaign_run_id = c.campaign_run_id
LEFT JOIN 
	(
	SELECT 
		c1.signup_id,
		max(CASE WHEN c1.post_id <> -1 THEN 1 ELSE 0 END) reportback
	FROM campaign_activity c1
	WHERE c1.campaign_run_id IN (7439,7657,7663,7832,7908)
	GROUP BY c1.signup_id
	) reportbacks ON reportbacks.signup_id = c.signup_id
WHERE c.campaign_run_id IN (7439,7657,7663,7832,7908)
GROUP BY c.campaign_run_id;