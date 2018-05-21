SELECT 
	count(*)
FROM derived_user_test u
;

SELECT 
	u.active_member,
	count(*)
FROM derived_user_test u
GROUP BY u.active_member;

SELECT 
	l.event_type,
	count(*)
FROM cio_latest_status l
GROUP BY l.event_type
;

SELECT 
	l.event_type,
	count(*)
FROM legacy_cio_status_import l
GROUP BY l.event_type;

SELECT 
	 u.customer_io_subscription_status,
	 count(*)
FROM northstar.users_mysql u
GROUP BY u.customer_io_subscription_status;

SELECT * FROM northstar.users_mysql u WHERE u.customer_io_subscription_status = 'subscribed' AND u.customer_io_subscription_timestamp IS NULL ;

SELECT 
	*
FROM northstar.users_mysql u
LEFT JOIN public.cio_latest_status c ON u.northstar_id = c.customer_id
WHERE u.customer_io_subscription_status IS NOT NULL 
AND c.customer_id IS NULL ;

SELECT * FROM cio_latest_status