DROP TABLE IF EXISTS public.legacy_cio_status_import;
CREATE TABLE public.legacy_cio_status_import AS 
	(SELECT 
		'' AS email_id,
		u.northstar_id AS customer_id, 
		u.email AS email_address, 
		NULL AS template_id,
		NULL AS event_id,
		u.customer_io_subscription_timestamp AS "timestamp", 
		CASE 
			WHEN u.customer_io_subscription_status = 'subscribed' THEN 'customer_subscribed'
			ELSE 'customer_unsubscribed' END AS event_type
	FROM northstar.users_mysql u 
	WHERE u.customer_io_subscription_status IS NOT NULL 
	AND customer_io_subscription_timestamp IS NOT NULL
	)
;

ALTER TABLE public.legacy_cio_status_import ADD PRIMARY KEY (customer_id, "timestamp", event_type);
CREATE INDEX leg_cio_indices ON public.legacy_cio_status_import (customer_id, "timestamp", event_type);
