SELECT
	c.northstar_id,
	c.post_id,
	c.status,
	c.caption,
	c.post_source
FROM quasar.users u
LEFT JOIN quasar.campaign_activity c ON u.northstar_id = c.northstar_id
WHERE c.campaign_run_id = 7890
AND u.email NOT LIKE '%dosomething.org%'
limit 50
;
