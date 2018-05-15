SELECT * FROM northstar.users ;
SELECT * FROM northstar.users_log_mysql;

SELECT 
	ul.northstar_id AS id,
	ul.first_name,
	ul.last_name,
	'' AS last_initial,
	NULL AS photo,
	ul.email,
	ul.facebook_id,
	ul.birthdate,
	ul.addr_street1,
	ul.addr_street2,
	ul.addr_city,
	ul.addr_state,
	ul.addr_zip,
	NULL AS ul.addr_source,
	ul."source",
	ul.source_detail,
	NULL AS slack_id,
	ul.sms_status,
	false AS sms_paused,
	ul."language",
	ul.country,
	ul.drupal_uid,
	NULL AS ul."role",
	ul.last_accessed AS last_accessed_at,
	ul.last_logged_in AS last_authenticated_at,
	NULL AS last_messaged_at,
	ul.created_at AS updated_at,
	NULL AS created_at
FROM northstar.users_log_mysql ul 
;

