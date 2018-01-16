#https://trello.com/c/hhvPK9Rq/1185-hawaii-data
SELECT 
	users.*,
	c.signup_id,
	max(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) reportback,
	i.campaign_node_id_title AS campaign,
	date(c.signup_created_at) as signup_date,
	i.campaign_type,
	i.campaign_action_type AS action_type,
	i.campaign_cause_type AS cause_type
FROM 
	(SELECT DISTINCT 
		u.northstar_id,
		u.source,
		FLOOR(DATEDIFF(CURRENT_DATE(), u.birthdate) / 365) age,
      	DATEDIFF(date(now()), date(u.created_at)) as 'days_a_member',
		COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state
	FROM quasar.users u
	LEFT JOIN quasar.moco_profile_import m 
	  ON (m.moco_id = u.moco_commons_profile_id AND u.moco_commons_profile_id IS NOT NULL)
	WHERE (u.customer_io_subscription_status = 'subscribed' 
		OR u.sms_status = 'active')
	AND u.country = 'US') users
LEFT JOIN quasar.campaign_activity c ON users.northstar_id=c.northstar_id
LEFT JOIN quasar.campaign_info i ON i.campaign_run_id = c.campaign_run_id 
WHERE users.addr_state = 'HI' OR users.addr_state LIKE '%hawaii%' OR users.addr_state = '{value: HI}'
GROUP BY c.signup_id
;