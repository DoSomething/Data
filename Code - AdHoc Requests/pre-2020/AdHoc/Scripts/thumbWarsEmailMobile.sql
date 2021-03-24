#https://trello.com/c/hK4zucJx/1100-thumb-wars-data-request
SELECT 
	u.northstar_id,
	u.email,
	u.mobile
FROM users u
INNER JOIN 
	(SELECT  
		c.northstar_id,
		max(CASE WHEN c.campaign_run_id = 7811 THEN 1 ELSE 0 END) AS tw
	FROM campaign_activity c
	WHERE c.signup_created_at >= '2017-06-01' AND c.signup_created_at <= '2017-07-05'
	GROUP BY c.northstar_id) camps
	ON camps.northstar_id = u.northstar_id AND camps.tw = 1
WHERE u.email IS NOT NULL
AND u.mobile IS NOT NULL
;