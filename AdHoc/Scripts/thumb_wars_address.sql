SELECT 
	quasar_users.first_name  AS `quasar_users.first_name`,
	quasar_users.last_name  AS `quasar_users.last_name`,
	quasar_users.addr_street1  AS `quasar_users.addr_street1`,
	quasar_users.addr_city  AS `quasar_users.addr_city`,
	quasar_users.addr_state  AS `quasar_users.addr_state`,
	quasar_users.addr_zip  AS `quasar_users.addr_zip`,
	quasar_users.northstar_id  AS `quasar_users.northstar_id`,
	dosomething_signup.signup_created_at
FROM campaign_activity  AS dosomething_signup
LEFT JOIN quasar.users  AS quasar_users ON dosomething_signup.northstar_id = quasar_users.northstar_id

WHERE (dosomething_signup.campaign_run_id  = 7811) 
	AND (NOT (dosomething_signup.signup_id  IS NULL)) 
	AND ((dosomething_signup.signup_created_at >= '2017-07-06')) 
	AND ((quasar_users.addr_street1 IS NOT NULL AND LENGTH(quasar_users.addr_street1 ) <> 0 ) 
	AND (quasar_users.addr_street1 IS NOT NULL))
GROUP BY 1,2,3,4,5,6,7,8
ORDER BY DATE(FROM_UNIXTIME(dosomething_signup.signup_created_at)
      ) DESC
      ;
      
SELECT 
	quasar_users.first_name  AS `quasar_users.first_name`,
	quasar_users.last_name  AS `quasar_users.last_name`,
	quasar_users.addr_street1  AS `quasar_users.addr_street1`,
	quasar_users.addr_city  AS `quasar_users.addr_city`,
	quasar_users.addr_state  AS `quasar_users.addr_state`,
	quasar_users.addr_zip  AS `quasar_users.addr_zip`,
	quasar_users.northstar_id  AS `quasar_users.northstar_id`,
	DATE(FROM_UNIXTIME(dosomething_signup.timestamp)
      ) AS `dosomething_signup.timestamp_date`
FROM dosomething.dosomething_signup  AS dosomething_signup
LEFT JOIN quasar.users  AS quasar_users ON dosomething_signup.uid = quasar_users.drupal_uid 

WHERE (dosomething_signup.run_nid  = 7811) 
	AND (NOT (dosomething_signup.sid  IS NULL)) 
	AND ((FROM_UNIXTIME(dosomething_signup.timestamp) >= TIMESTAMP('2017-07-06'))) 
	AND ((quasar_users.addr_street1 IS NOT NULL 
	AND LENGTH(quasar_users.addr_street1 ) <> 0 ) 
	AND (quasar_users.addr_street1 IS NOT NULL)) 
	AND (DATE(FROM_UNIXTIME(dosomething_signup.timestamp))>='2000-01-01')
GROUP BY 1,2,3,4,5,6,7,8
ORDER BY DATE(FROM_UNIXTIME(dosomething_signup.timestamp)
      ) DESC
LIMIT 500