#https://trello.com/c/vGAxjQVV/1101-ctd-driving-coach-data-request
SELECT 
	u.northstar_id,
	u.email,
	u.mobile,
	camps.ctd,
	camps.dc
FROM users u
INNER JOIN 
	(SELECT  
		c.northstar_id,
		max(CASE WHEN c.campaign_run_id = 7702 THEN 1 ELSE 0 END) AS ctd,
		max(CASE WHEN c.campaign_run_id = 6093 THEN 1 ELSE 0 END) AS dc
	FROM campaign_activity c
	GROUP BY c.northstar_id) camps
	ON camps.northstar_id = u.northstar_id AND (camps.ctd = 1 OR camps.dc = 1)
WHERE 
(camps.ctd = 1 AND u.email IS NOT NULL AND u.mobile IS NOT NULL) OR 
(camps.ctd = 0)
;