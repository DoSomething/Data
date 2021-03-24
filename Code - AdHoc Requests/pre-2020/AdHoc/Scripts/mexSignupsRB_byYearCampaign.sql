--https://trello.com/c/rczVQ2DE/1158-mexico-quarterly-updates
SELECT 
	ca."year",
	ca.campaign_run_id,
	i.campaign_node_id_title,
	count(*) AS signups,
	sum(ca.reportbacks) AS reportbacks
FROM
	(SELECT 
		c.signup_id,
		c.campaign_run_id,
		EXTRACT('year' FROM c.signup_created_at) AS "year",
		max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) AS reportbacks
	FROM public.users u
	LEFT JOIN public.campaign_activity c ON u.northstar_id=c.northstar_id
	WHERE u.country='MX'
	AND c.signup_created_at >= '2016-01-01'
	GROUP BY 1,2,3) ca
LEFT JOIN public.campaign_info i ON ca.campaign_run_id = i.campaign_run_id
GROUP BY 1,2,3
;

SELECT count(*) AS count_active_users FROM public.users u WHERE u.subscribed_member = TRUE AND u.country='MX' 

;
SELECT 
	c.signup_id,
	c.post_id,
	c.post_created_at,
	c.url
FROM public.users u
INNER JOIN public.campaign_activity c ON u.northstar_id=c.northstar_id
WHERE u.country='MX'
AND c.post_id <> -1 
AND c.signup_created_at >= '2016-01-01'
;