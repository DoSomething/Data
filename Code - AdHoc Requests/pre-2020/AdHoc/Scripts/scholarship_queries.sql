#Pull scholarship winners (weighted):
SELECT 
	candidates.*,
	camps.status AS 'report_back_status'
FROM 
	(SELECT 
		ca.signup_id, 
		ca.campaign_run_id, 
		ca.signup_created_at, 
		ca.northstar_id, 
		u.first_name, 
		u.email, 
		u.mobile, 
		if((UNIX_TIMESTAMP(ca.signup_created_at) - UNIX_TIMESTAMP(u.`created_at`)) <= 300,1,0) AS 'new_member', 
		MAX(ca.quantity) as 'amount_reported_back'
	FROM quasar.campaign_activity ca 
	LEFT OUTER JOIN quasar.users u 
	    ON u.northstar_id = ca.northstar_id 
	WHERE ca.campaign_run_id = RUN_ID
	AND ca.quantity >= QUANTITY
	GROUP BY ca.northstar_id) candidates
LEFT JOIN quasar.campaign_activity camps ON camps.northstar_id = candidates.northstar_id
WHERE candidates.amount_reported_back > 0
ORDER BY -log(rand(57))/truncate(candidates.amount_reported_back/1,0) ASC
LIMIT 30
;;

#Pull scholarship winners (non-weighted):
SELECT 
	winners.*,
	camps.status AS 'report_back_status'
FROM
	(SELECT 
		ca.signup_id, 
		ca.campaign_run_id, 
		ca.signup_created_at, 
		ca.northstar_id,
		u.first_name,
		u.email, 
		u.mobile, 
		if((UNIX_TIMESTAMP(ca.signup_created_at) - UNIX_TIMESTAMP(u.`created_at`)) <= 300,1,0) AS 'new_member', 
		SUM(ca.quantity) AS total_quantity
	FROM quasar.campaign_activity ca 
	LEFT OUTER JOIN quasar.users u 
	    ON u.northstar_id = ca.northstar_id 
	WHERE ca.campaign_run_id = RUN_ID 
	AND ca.quantity >= QUANTITY
	GROUP BY u.northstar_id
	ORDER BY 
		-log(rand(82)) 
	LIMIT 30) winners
LEFT JOIN quasar.campaign_activity camps ON camps.northstar_id = winners.northstar_id
;
