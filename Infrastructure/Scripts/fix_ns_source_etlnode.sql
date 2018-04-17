SELECT 
	u.northstar_id,
	u.email,
	u.source,
	COALESCE(m.email, i.email) AS email,
	COALESCE(m.confirm_time, i.confirm_time) AS created_at
FROM quasar.users u 
LEFT JOIN mailchimp_final_exports.final_mailchimp_main_sub m ON m.email = u.email
LEFT JOIN mailchimp_final_exports.final_mailchimp_intl_sub i ON i.email = u.email 
WHERE u.source LIKE '%etl%'
AND u.created_at >= '2017-04-01' AND u.created_at < '2017-05-01';