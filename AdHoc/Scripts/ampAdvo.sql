SELECT 
	camp.northstar_id,
	max(CASE WHEN camp.`campaign_run_id` IN (7663, 7772) THEN 1 ELSE 0 END) AS participated_in_amplify,
	max(CASE WHEN camp.`campaign_run_id` IN (7664, 7803) THEN 1 ELSE 0 END) AS participated_in_advocate,
	count(DISTINCT non_advoamp_camps.signup_id) AS total_campaigns_post_march
FROM campaign_activity camp 
LEFT JOIN campaign_activity non_advoamp_camps 
	ON camp.`northstar_id` = non_advoamp_camps.`northstar_id` 
	AND (non_advoamp_camps.signup_created_at >= '2017-03-01' AND non_advoamp_camps.`campaign_run_id` NOT IN (7663, 7772, 7664, 7803))
WHERE camp.`campaign_run_id` IN (7663, 7772, 7664, 7803)
GROUP BY camp.northstar_id
LIMIT 400;
