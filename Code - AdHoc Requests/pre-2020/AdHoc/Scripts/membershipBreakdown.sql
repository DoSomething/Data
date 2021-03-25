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