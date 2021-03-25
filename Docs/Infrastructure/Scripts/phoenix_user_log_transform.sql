DROP TABLE IF EXISTS qtmp.phoenix_user_log_transform;

CREATE TABLE qtmp.phoenix_user_log_transform
(
northstar_id VARCHAR(255),
first_name VARCHAR(255),
email VARCHAR(255),
created_at_timestamp DATETIME,
last_accessed DATETIME,
last_logged_in DATETIME,
`language` VARCHAR(255)
);

INSERT INTO qtmp.phoenix_user_log_transform
SELECT
	u.northstar_id,
	plog.name AS first_name,
	plog.mail AS email,
	FROM_UNIXTIME(plog.created) AS created_at_timestamp,
	FROM_UNIXTIME(plog.access) AS last_accessed,
	FROM_UNIXTIME(plog.login) AS last_logged_in,
	plog.language
FROM quasar.phoenix_user_log_poc plog
LEFT JOIN quasar.users u ON u.drupal_uid = plog.uid
WHERE FROM_UNIXTIME(plog.access) <= '2017-07-30'
AND u.northstar_id IS NOT NULL ;

SELECT * FROM qtmp.phoenix_user_log_transform LIMIT 400;