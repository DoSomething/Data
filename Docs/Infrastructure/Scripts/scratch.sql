SELECT COLUMN_NAME
FROM information_schema.columns c
WHERE c.TABLE_SCHEMA='quasar'
AND table_name='users';

SHOW processlist;

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


SELECT 
	count(*) AS total_active,
	sum(CASE WHEN sms_status='active' AND 
		(u.customer_io_subscription_status <> 'subscribed' OR
		u.customer_io_subscription_status IS NULL) 
	THEN 1 ELSE 0 END) AS sms_only,
	sum(CASE WHEN source='niche' THEN 1 ELSE 0 END) AS niche,
	sum(CASE WHEN (sms_status<>'active' OR
					sms_status IS null) 
				  AND u.customer_io_subscription_status = 'subscribed' 
		THEN 1 ELSE 0 END) AS email_only,
	sum(CASE WHEN sms_status='active' AND u.customer_io_subscription_status = 'subscribed' THEN 1 ELSE 0 END) AS 'both'
FROM quasar.users u
WHERE (u.sms_status = 'active' OR
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

SELECT
   MAX(CASE WHEN GREATEST(u.last_accessed, u.last_logged_in) <= u.created_at 
     AND u.source='niche' 
     AND COUNT(DISTINCT c.signup_id), 0 <= 1
     AND COUNT(DISTINCT c.post_id), 0 <= 1
  THEN 1 ELSE 0 END) as bad_niche
FROM quasar.users u
LEFT JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
GROUP BY u.northstar_id;

SELECT 
	u.northstar_id,
	u.mobile,
	u.email,
	rbs_7890.reportback AS within_campaign_rbs,
	non_7890_rbs.reportback AS outside_campaign_rbs,
	IFNULL(rbs_7890.reportback, 0) + IFNULL(non_7890_rbs.reportback, 0) AS total_reportbacks
FROM quasar.users u 
LEFT JOIN 
	(SELECT 
		rbs.northstar_id,
		sum(rbs.reportback) AS reportback
	FROM 
		(SELECT 
			c.northstar_id,
			c.signup_id,
			c.campaign_run_id,
			max(CASE WHEN c.post_id <> -1 AND c.status='approved'  THEN 1 ELSE 0 END) AS reportback
		FROM quasar.campaign_activity c
		GROUP BY c.northstar_id, c.signup_id) rbs
	WHERE rbs.campaign_run_id <> 7890
	GROUP BY rbs.northstar_id) non_7890_rbs
	ON non_7890_rbs.northstar_id = u.northstar_id
LEFT JOIN 
	 (SELECT 
	 	c1.northstar_id,
	 	c1.signup_id,
	 	sum(CASE WHEN c1.post_id <> -1 AND c1.status='approved'  THEN 1 ELSE 0 END) AS reportback
	 FROM quasar.campaign_activity c1
	 WHERE c1.campaign_run_id = 7890
	 GROUP BY c1.northstar_id
	 ) rbs_7890
ON u.northstar_id = rbs_7890.northstar_id
LIMIT 200;

;WITH tblDifference AS
(
    SELECT ROW_NUMBER() OVER(ORDER BY `timestamp`) AS RowNumber, columnOfNumbers 
    FROM quasar.monitoring
    WHERE query='active_user_count'
)

SELECT cur.columnOfNumbers, cur.columnOfNumbers - previous.columnOfNumbers
FROM tblDifference cur
LEFT OUTER JOIN tblDifference previous
ON cur.RowNumber = previous.RowNumber + 1
;

SELECT 
	*
FROM 
	(SELECT 
		c.northstar_id,
		c.campaign_run_id
		sum(case when c.post_id <> -1 then 1 else 0 end) as reported_backsquasar.campaign_activity c)
	FROM quasar.campaign_activity c 
	WHERE c.campaign_run_id = 7916
	GROUP BY c.northstar_id) camp
LEFT JOIN
	(SELECT 
		ul.northstar_id,
		sum(case when ul.last_accessed >= '2017-09-01' AND ul.last_accessed <= '2017-10-31' then 1 else 0 end ) as site_visits
	FROM quasar.users_log ul
	GROUP BY ul.northstar_id) userlog
ON camp.northstar_id=userlog.northstar_id
WHERE 
c.northstar_id IN ('565e25c7469c64a8178b7cf5');


SELECT DISTINCT 
	c.northstar_id,
	c.signup_created_at,
	u.addr_state,
	u.addr_zip,
	CASE WHEN mel.action_ts >= c.signup_created_at THEN 0 ELSE 1 END AS new_member
FROM quasar.campaign_activity c
INNER JOIN quasar.users u ON c.northstar_id = u.northstar_id
INNER JOIN 
	(SELECT 
		m.northstar_id,
		min(m.`timestamp`) AS action_ts
	FROM quasar.campaign_activity camp
	LEFT JOIN quasar.member_event_log m ON camp.northstar_id = m.northstar_id
	WHERE camp.campaign_run_id=7931 
	GROUP BY m.northstar_id) mel
ON mel.northstar_id = c.northstar_id
LIMIT 500;

SELECT 
	winners.*,
	camps.status AS 'report_back_status'
FROM
	(SELECT 
		ca.signup_id, 
		ca.campaign_run_id, 
		ca.signup_created_at, 
		ca.northstar_id,
		u.first_name,
		u.email, 
		u.mobile, 
		if((UNIX_TIMESTAMP(ca.signup_created_at) - UNIX_TIMESTAMP(u.`created_at`)) <= 300,1,0) AS 'new_member', 
		SUM(ca.quantity) AS total_quantity
	FROM quasar.campaign_activity ca 
	LEFT OUTER JOIN quasar.users u 
	    ON u.northstar_id = ca.northstar_id 
	WHERE ca.campaign_run_id = 8043 
	AND ca.quantity >= 0
	GROUP BY u.northstar_id
	ORDER BY 
		-log(rand(57)) 
	LIMIT 30) winners
LEFT JOIN quasar.campaign_activity camps ON camps.northstar_id = winners.northstar_id
LIMIT 50;


SELECT 
	camp_count.dte,
	count(DISTINCT camp_count.northstar_id)
FROM 
	(SELECT 
		c.northstar_id,
		date(c.signup_created_at) AS dte,
		count(*)
	FROM campaign_activity c
	GROUP BY c.northstar_id, c.signup_id, date(c.signup_created_at)
	HAVING count(*) > 1
	LIMIT 1000
	) camp_count
GROUP BY camp_count.dte
;

SELECT 
	count(*),
	6000000-count(*)
FROM quasar.users u
WHERE u.customer_io_subscription_status = 'subscribed' 
      OR u.sms_status = 'active';

SET @rank=0;
SELECT *
FROM 
	(SELECT 
		@rank:=@rank+1 AS pos, 
		u.northstar_id, 
		u.created_at,
		u.source,
		u.email,
		u.mobile,
		u.first_name,
		u.last_name,
		u.addr_city,
		u.addr_state,
		u.country
	FROM quasar.users u 
	WHERE (u.customer_io_subscription_status = 'subscribed' 
	      OR u.sms_status = 'active')
	ORDER BY u.created_at) u_ord
WHERE u_ord.pos = 6000000
;

SELECT count(*) 
FROM cio.event_log e 
WHERE e.northstar_id = '592f76eba0bfad17c42ab36e' 
AND JSON_EXTRACT(e.`data`, "$.event_type") = 'customer_unsubscribed';

SELECT DISTINCT JSON_EXTRACT(e.`data`, "$.event_type") 
FROM (SELECT * FROM cio.event_log et LIMIT 10000) e;

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

SELECT 
	count(*),
	sum(CASE WHEN m.email IS NULL AND i.email IS NULL THEN 1 ELSE 0 end) AS count_unmatched	
FROM quasar.users u 
LEFT JOIN mailchimp_final_exports.final_mailchimp_main_sub m ON m.email = u.email
LEFT JOIN mailchimp_final_exports.final_mailchimp_intl_sub i ON i.email = u.email 
WHERE u.source LIKE '%etl%'
AND u.created_at >= '2017-04-01' AND u.created_at < '2017-05-01';

SELECT 
	u.facebook_id
FROM quasar.users u 
WHERE u.facebook_id IS NOT NULL AND u.facebook_id <> '';

SELECT * FROM campaign_info i WHERE i.campaign_node_id_title LIKE '%gun%';

SELECT * FROM quasar.member_event_log;

SELECT count(*) FROM quasar.users u WHERE u.country = 'MX'