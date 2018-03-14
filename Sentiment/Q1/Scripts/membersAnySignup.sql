SELECT
	su_break.niche,
	su_break.sms_only,
	su_break.has_signup,
	count(*)
FROM
	(SELECT
		u.northstar_id,
		CASE WHEN source='niche' THEN 1 ELSE 0 END AS niche,
		CASE WHEN (u.customer_io_subscription_status IS NULL OR u.customer_io_subscription_status = 'subscribed')
	    	AND (u.email IS NULL OR LENGTH(u.email) = 0) AND u.mobile IS NOT NULL AND u.sms_status = 'active'
	        THEN 1 ELSE 0 END AS sms_only,
		CASE WHEN count(DISTINCT c.signup_id) > 0 THEN 1 ELSE 0 END AS has_signup
	FROM quasar.users u
	LEFT JOIN quasar.campaign_activity c ON u.northstar_id=c.northstar_id
	WHERE u.sms_status='active' OR u.customer_io_subscription_status='subscribed'
	GROUP BY u.northstar_id) su_break
GROUP BY su_break.niche, su_break.sms_only, su_break.has_signup
;
/*
SELECT
	su_break.niche,
	su_break.sms_only,
	su_break.has_signup,
	count(*)
FROM
	(SELECT
		u.northstar_id,
		CASE WHEN source='niche' THEN 1 ELSE 0 END AS niche,
		CASE WHEN (u.customer_io_subscription_status IS NULL OR u.customer_io_subscription_status = 'subscribed')
	    	AND (u.email IS NULL OR LENGTH(u.email) = 0) AND u.mobile IS NOT NULL AND u.sms_status = 'active'
	        THEN 1 ELSE 0 END AS sms_only,
		CASE WHEN count(DISTINCT c.signup_id) > 0 THEN 1 ELSE 0 END AS has_signup
	FROM quasar.users u
	LEFT JOIN quasar.campaign_activity c ON u.northstar_id=c.northstar_id
	WHERE u.sms_status='active' OR u.customer_io_subscription_status='subscribed'
	GROUP BY u.northstar_id) su_break
GROUP BY su_break.niche, su_break.sms_only, su_break.has_signup;