DROP TABLE IF EXISTS `quasar_user_log_poc_test`;

CREATE TABLE qtmp.quasar_user_log_poc_test
(
user_id VARCHAR(255),
user_last_accessed DATETIME,
user_last_logged_in DATETIME,
user_created_at_timestamp DATETIME,
phoenix_drupal_uid INT,
phoenix_name VARCHAR(255),
phoenix_email VARCHAR(255),
phoenix_created_at_timestamp DATETIME,
phoenix_last_accessed DATETIME, 
phoenix_last_logged_in DATETIME, 
`status` VARCHAR(255),
timezone VARCHAR(255),
phoenix_language VARCHAR(255),
northstar_id VARCHAR(255),
northstar_created_at_timetamp DATETIME,
last_logged_in DATETIME,
last_accessed DATETIME,
drupal_uid INT,
northstar_id_source_name VARCHAR(255),
email VARCHAR(255),
mobile VARCHAR(26),
birthdate DATETIME,
first_name VARCHAR(255),
last_name VARCHAR(255),
addr_street1 VARCHAR(255),
addr_street2 VARCHAR(255),
addr_city VARCHAR(255),
addr_state VARCHAR(26),
addr_zip VARCHAR(26),
country VARCHAR(26),
`language` VARCHAR(26),
agg_id INT,
cgg_id INT,
customer_io_subscription_status VARCHAR(32),
customer_io_subscription_timestamp DATETIME,
mailchimp_first_subscribed DATETIME,
mailchimp_unsubscribed_time DATETIME,
mailchimp_subscription_status VARCHAR(16),
mailchimp_list_id VARCHAR(24),
mailchimp_avg_open_rate FLOAT,
mailchimp_avg_click_rate FLOAT,
mailchimp_latitude FLOAT,
mailchimp_longitude FLOAT,
mailchimp_country_code VARCHAR(4),
moco_commons_profile_id VARCHAR(24),
moco_current_status VARCHAR(48),
moco_source_detail VARCHAR(96) 
)
;

INSERT INTO qtmp.quasar_user_log_poc_test
SELECT 
	CONCAT(COALESCE(joint.phoenix_drupal_uid,''), COALESCE(joint.northstar_id,'')) AS user_id,
	COALESCE(joint.phoenix_last_accessed, joint.last_accessed) AS user_last_accessed,
	COALESCE(joint.phoenix_last_logged_in, joint.last_logged_in) AS user_last_logged_in,
	min_times.user_created_at_timestamp,
	joint.*
FROM 
	(SELECT
		p1.uid AS phoenix_drupal_uid,
		p1.name AS phoenix_name,
		p1.mail AS phoenix_email,
		FROM_UNIXTIME(p1.created) AS phoenix_created_at_timestamp,
		FROM_UNIXTIME(p1.access) AS phoenix_last_accessed,
		FROM_UNIXTIME(p1.login) AS phoenix_last_logged_in,
		p1.status,
		p1.timezone,
		p1.language as phoenix_language,
		u1.*
	FROM quasar.phoenix_user_log_poc p1
	LEFT JOIN quasar.users_log u1 ON p1.uid = u1.drupal_uid AND u1.last_accessed = from_unixtime(p1.access)
	UNION 
	SELECT
		p2.uid AS phoenix_drupal_uid,
		p2.name AS phoenix_name,
		p2.mail AS phoenix_email,
		FROM_UNIXTIME(p2.created) AS phoenix_created_at_timestamp,
		FROM_UNIXTIME(p2.access) AS phoenix_last_accessed,
		FROM_UNIXTIME(p2.login) AS phoenix_last_logged_in,
		p2.status,
		p2.timezone,
		p2.language AS phoenix_language,
		u2.*
	FROM quasar.phoenix_user_log_poc p2
	RIGHT JOIN quasar.users_log u2 ON p2.uid = u2.drupal_uid AND u2.last_accessed = from_unixtime(p2.access)
	) joint
LEFT JOIN 
	(SELECT 
		timtemp1.user_id,
		min(timtemp1.user_created_ts) AS user_created_at_timestamp
	FROM 
		(SELECT 
			CONCAT(COALESCE(timtemp.drupal_uid,''), COALESCE(timtemp.northstar_id,'')) AS user_id,
			COALESCE(timtemp.phoenix_created_at_timestamp, timtemp.northstar_created_at_timestamp) AS user_created_ts
		FROM 	
			(SELECT 
				ptim1.uid AS drupal_uid,
				FROM_UNIXTIME(ptim1.created) AS phoenix_created_at_timestamp,
				utim1.northstar_id,
				utim1.northstar_created_at_timestamp
			FROM quasar.phoenix_user_log_poc ptim1
			LEFT JOIN quasar.users_log utim1 ON ptim1.uid = utim1.drupal_uid AND utim1.last_accessed = from_unixtime(ptim1.access)
			UNION ALL
			SELECT
				ptim2.uid AS drupal_uid,
				FROM_UNIXTIME(ptim2.created) AS phoenix_created_at_timestamp,
				utim2.northstar_id,
				utim2.northstar_created_at_timestamp
				FROM quasar.phoenix_user_log_poc ptim2
			RIGHT JOIN quasar.users_log utim2 ON ptim2.uid = utim2.drupal_uid AND utim2.last_accessed = from_unixtime(ptim2.access)
			) timtemp
		) timtemp1
	GROUP BY timtemp1.user_id
	) min_times
	ON min_times.user_id = CONCAT(COALESCE(joint.phoenix_drupal_uid,''), COALESCE(joint.northstar_id,''))
; 

