SELECT COLUMN_NAME
FROM information_schema.columns c
WHERE c.TABLE_SCHEMA='quasar'
AND table_name='users';

SELECT *
FROM quasar.users u 
INNER JOIN (
	SELECT 
		max(t.northstar_created_at_timestamp) AS max_created
	FROM quasar.users t
	) m ON m.max_created = u.northstar_created_at_timestamp;

CREATE TEMPORARY TABLE IF NOT EXISTS max_user_created AS (
	SELECT 
		max(t.northstar_created_at_timestamp) AS max_created
	FROM quasar.users t
	);
	
SELECT * FROM quasar.users LIMIT 500;
SELECT * FROM quasar.monitoring LIMIT 100;
SELECT * FROM gladiator.competitions LIMIT 500;
SELECT * FROM gladiator.competitions LIMIT 500;
SELECT * FROM gladiator.users LIMIT 200;

SHOW processlist
;

SELECT * FROM quasar.monitoring WHERE ;

SELECT * FROM 
	(SELECT 
		c.signup_id,
		max(CASE WHEN c.status = 'accepted' THEN 1 ELSE 0 end) AS any_accepted,
		max(CASE WHEN c.status = 'rejected' THEN 1 ELSE 0 end) AS any_rejected
	FROM quasar.campaign_activity c
	GROUP BY c.signup_id) a
WHERE a.any_accepted=1 AND a.any_rejected=1
;

SELECT * FROM quasar.monitoring;

SELECT  	m.INDEX,
               m.query,  
               m.output  
            FROM quasar.monitoring m   
            WHERE m.table = 'quasar.users' 
            AND m.timestamp = (SELECT max(t1.timestamp)  
                               FROM quasar.monitoring t1 
                               WHERE t1.query = 'user_count') 
            OR m.timestamp = (SELECT max(t1.timestamp)  
                               FROM quasar.monitoring t1 
                               WHERE t1.query = 'user_distinct_user_count')
;

SELECT * FROM campaign_activity LIMIT 500;

SELECT 
	count(DISTINCT c.signup_id) AS signups,
	sum(reportbacks.reportback) AS reportbacks,
	CASE WHEN c.campaign_run_id IN (7663, 7832, 7908) THEN 'Social Share'
		ELSE 'Card Making' END AS TYPE,
	c.campaign_run_id,
	info.campaign_run_start_date AS start_date,
	info.campaign_run_id_title campaign
FROM campaign_activity c
LEFT JOIN campaign_info info ON info.campaign_run_id = c.campaign_run_id
LEFT JOIN 
	(
	SELECT 
		c1.signup_id,
		max(CASE WHEN c1.post_id <> -1 THEN 1 ELSE 0 END) reportback
	FROM campaign_activity c1
	WHERE c1.campaign_run_id IN (7439,7657,7663,7832,7908)
	GROUP BY c1.signup_id
	) reportbacks ON reportbacks.signup_id = c.signup_id
WHERE c.campaign_run_id IN (7439,7657,7663,7832,7908)
GROUP BY c.campaign_run_id;

SELECT * FROM campaign_activity LIMIT 500
;

	
SELECT 
	count(*)
FROM 
	(SELECT 
		*
	FROM 
		(SELECT 
			log_sub.northstar_id,
			max(CASE WHEN log_sub.moco_current_status = 'active' THEN 1 ELSE 0 END) eversub
		FROM quasar.users_log log_sub
		WHERE log_sub.moco_current_status IS NOT NULL
		AND log_sub.last_accessed IS NOT NULL
		GROUP BY log_sub.northstar_id) temp_sub
	WHERE temp_sub.eversub = 1) eversub
INNER JOIN 
	(SELECT 
		* 
	FROM 
		(SELECT 
			log_unsub.northstar_id,
			max(CASE WHEN log_unsub.moco_current_status <> 'active' THEN 1 ELSE 0 END) unsubscribed_2017
		FROM quasar.users_log log_unsub
		WHERE log_unsub.moco_current_status IS NOT NULL
		AND log_unsub.last_accessed IS NOT NULL
		AND log_unsub.last_accessed >= '2017-01-01'
		GROUP BY log_unsub.northstar_id) temp_unsub
	WHERE temp_unsub.unsubscribed_2017 = 1) unsub_2017 
ON unsub_2017.northstar_id = eversub.northstar_id
;

SELECT 
 count(*)
FROM quasar.users u 
LEFT JOIN quasar.moco_profile_import ON m.moco_id = u.moco_commons_profile_id
WHERE u.moco_current_status = 'active' 
OR  u.customer_io_subscription_status = 'subscribed';

SELECT * FROM quasar.campaign_activity c WHERE c.campaign_run_id=7931 LIMIT 200 ;

SELECT 
	count(*)
FROM 
	(SELECT 
	  COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state
	FROM quasar.users u 
	LEFT JOIN quasar.moco_profile_import m ON m.moco_id = u.moco_commons_profile_id
	#WHERE 
	#(u.moco_current_status = 'active' 
	#OR u.customer_io_subscription_status = 'subscribed') 
	) state
#WHERE 
#(state.addr_state LIKE '%TX%' 
#OR state.addr_state LIKE '%tex%')
#state.addr_state IS NULL 
; #Texas active verified 400,909 #Texas total verified 586,339 #No State 3,266,848 #Records 9,440,476

SELECT * FROM quasar.campaign_info i WHERE i.campaign_node_id_title LIKE '%card%' LIMIT 50;
SELECT * FROM quasar.campaign_activity m LIMIT 100;
SELECT 
	c.campaign_run_id,
	date(c.signup_created_at) AS signup_date,
	count(*) AS signups
FROM quasar.campaign_activity c 
WHERE c.campaign_run_id IN (7060, 7944)
GROUP BY c.campaign_run_id
ORDER BY c.campaign_run_id;

SELECT * FROM  users_and_activities.mobile_master_lookup_lite LIMIT 50;
SELECT count(*) FROM quasar.users u WHERE (u.moco_current_status = 'active' OR
    u.customer_io_subscription_status = 'subscribed') LIMIT 100;
SELECT * FROM quasar.users LIMIT 100;


SELECT * FROM quasar.monitoring m WHERE m.`table` = 'quasar.campaign_activity' AND m.query = 'ca_table_count';
SELECT * FROM quasar.campaign_activity c WHERE post_id <> -1 LIMIT 50; 

SELECT 
	u.addr_state AS state,
	count(*),
	count(*) + (count(*)*.474)
FROM quasar.campaign_activity c
LEFT JOIN quasar.users u ON c.northstar_id = u.northstar_id
WHERE c.campaign_run_id=7931
GROUP BY u.addr_state;

SELECT * FROM users LIMIT 50;
GROUP BY u.state;

SELECT 
	count(*) AS total_active,
	sum(CASE WHEN moco_current_status='active' AND 
		(u.customer_io_subscription_status <> 'subscribed' OR
		u.customer_io_subscription_status IS NULL) 
	THEN 1 ELSE 0 END) AS sms_only,
	sum(CASE WHEN (moco_current_status<>'active' OR
					moco_current_status IS null) 
				  AND u.customer_io_subscription_status = 'subscribed' 
		THEN 1 ELSE 0 END) AS email_only,
	sum(CASE WHEN moco_current_status='active' AND u.customer_io_subscription_status = 'subscribed' THEN 1 ELSE 0 END) AS 'both'
FROM quasar.users u
WHERE (u.moco_current_status = 'active' OR
    u.customer_io_subscription_status = 'subscribed');
    
    
SELECT 
	u.northstar_id,
	a.signup_id,
	UNIX_TIMESTAMP(u.northstar_created_at_timestamp) AS created_at,
	DATE(u.birthdate) AS birthdate,
	u.first_name,
	u.last_name,
	u.`language`,
	m.mobile,
	m.status AS sms_status,
	u.northstar_id_source_name AS source,
	COALESCE(NULLIF(u.addr_street1, ''), NULLIF(m.addr_street1, '')) AS addr_street1,
	COALESCE(NULLIF(u.addr_street2, ''), NULLIF(m.addr_street2, '')) AS addr_street2,
	COALESCE(NULLIF(u.addr_city,''), NULLIF(m.addr_city,''), NULLIF(m.loc_city,'')) AS addr_city,
	COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state,
	COALESCE(NULLIF(u.addr_zip, ''), NULLIF(m.addr_zip, ''), NULLIF(m.loc_zip,'')) AS addr_zip,
	COALESCE(NULLIF(u.country, ''), NULLIF(m.addr_country,''), NULLIF(m.loc_country,'')) AS addr_country
FROM quasar.campaign_activity a
LEFT JOIN quasar.users u 
	ON a.northstar_id = u.northstar_id
LEFT JOIN quasar.moco_profile_import m ON m.moco_id = u.moco_commons_profile_id
WHERE u.moco_commons_profile_id IS NOT NULL
AND a.campaign_run_id = '7931';

SELECT * FROM quasar.moco_profile_import i LIMIT 50;
SELECT * FROM users_and_activities.mobile_users LIMIT 500;
SELECT u.northstar_id, u.mobile FROM quasar.users u WHERE u.northstar_id in ('59aedc53a0bfad10f547bdc8','59e73793a0bfad7f9319cb1d');

SELECT 
	count(*) AS total_users,
	sum(CASE WHEN sms_status='active' OR customer_io_subscription_status='subscribed' THEN 1 ELSE 0 END) active_members
FROM quasar.users u 
WHERE u.country='MX' LIMIT 600;

SELECT 
	u.country,
	count(*) AS total_users,
	sum(CASE WHEN sms_status='active' OR customer_io_subscription_status='subscribed' THEN 1 ELSE 0 END) active_members
FROM quasar.users u 
GROUP BY u.country;

SELECT 
	ca.`year`,
	ca.campaign_run_id,
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
GROUP BY ca.`year`, ca.campaign_run_id
;

SELECT DISTINCT 
	ca.post_id,
	ca.url
FROM 
	(SELECT 
		c.signup_id,
		c.post_id,
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
SELECT 
		c.signup_id,
		c.post_id,
		c.url,
		max(c.submission_updated_at) AS max_submission
	FROM quasar.users u
	INNER JOIN quasar.campaign_activity c ON u.northstar_id=c.northstar_id
	WHERE u.country='MX'
	AND c.post_id <> -1 
	AND c.signup_created_at >= '2016-01-01'
	GROUP BY c.signup_id;
	
SELECT * FROM campaign_activity WHERE post_id IN (151680,127764,128127,133430,136269,141884);

SELECT distinct log.source  FROM quasar.member_event_log log LIMIT 1000;
SELECT * FROM quasar.campaign_info i WHERE i.campaign_node_id_title LIKE '%ride%' ;

SELECT count(DISTINCT event_id)  FROM quasar.member_event_log LIMIT 50 = 29,545,434