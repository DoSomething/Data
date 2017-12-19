#https://trello.com/c/rczVQ2DE/1158-mexico-quarterly-updates
SELECT 
	ca.`year`,
	ca.campaign_run_id,
	count(*) AS signups,
	sum(ca.reportbacks) AS reportbacks
FROM
	(SELECT 
		c.signup_id,
		c.campaign_run_id,
		year(c.signup_created_at) AS 'year',
		max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) AS reportbacks
	FROM quasar.users u
	LEFT JOIN quasar.campaign_activity c ON u.northstar_id=c.northstar_id
	WHERE u.country='MX'
	AND c.signup_created_at >= '2016-01-01'
	GROUP BY c.signup_id) ca
GROUP BY ca.`year`, ca.campaign_run_id
;