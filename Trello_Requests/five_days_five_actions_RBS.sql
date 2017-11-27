#https://trello.com/c/JfzZVRLB/1109-5-days-5-actions-data-request
SELECT 
	c.northstar_id,
	c.post_id,
	c.status,
	c.caption,
	c.post_source
FROM users u
LEFT JOIN campaign_activity c ON u.northstar_id = c.northstar_id
WHERE c.campaign_run_id = 7890 
AND u.email NOT LIKE '%dosomething.org%'
;