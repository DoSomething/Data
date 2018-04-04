#https://trello.com/c/rczVQ2DE/1158-mexico-quarterly-updates
SELECT 
	ca.`year`,
	ca.campaign_run_id,
	i.campaign_node_id_title,
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
LEFT JOIN quasar.campaign_info i ON ca.campaign_run_id = i.campaign_run_id
GROUP BY ca.`year`, ca.campaign_run_id
;

SELECT DISTINCT 
	ca.post_id,
	ca.submission_created_at,
	ca.url
FROM 
	(SELECT 
		c.signup_id,
		c.post_id,
		c.submission_created_at,
		c.url,
		max(c.submission_updated_at) AS max_submission
	FROM quasar.users u
	INNER JOIN quasar.campaign_activity c ON u.northstar_id=c.northstar_id
	WHERE u.country='MX'
	AND c.post_id <> -1 
	AND c.signup_created_at >= '2016-01-01'
	GROUP BY c.signup_id) ca
INNER JOIN quasar.campaign_activity camp ON camp.signup_id = ca.signup_id AND camp.submission_updated_at = ca.max_submission
;