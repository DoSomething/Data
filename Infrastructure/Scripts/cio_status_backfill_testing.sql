DROP TABLE IF EXISTS mc_import;
CREATE TEMPORARY TABLE mc_import AS (
SELECT 
	'' AS email_id,
	u.northstar_id AS customer_id,
	mc.email AS email_address,
	NULL AS template_id,
	MD5(CONCAT(u.northstar_id, mc.email, mc.confirm_time)) AS event_id,
	mc.confirm_time AS "timestamp",
	mc.event_type
FROM 
	(
		SELECT 
			sub.drupal_uid,
			sub.email,
			sub.confirm_time,
			'customer_subscribed' AS event_type
		FROM mailchimp_final_exports.final_mailchimp_main_sub sub
	UNION ALL 
		SELECT 
			isub.drupal_uid,
			isub.email,
			isub.confirm_time,
			'customer_subscribed' AS event_type
		FROM mailchimp_final_exports.final_mailchimp_intl_sub isub
	UNION ALL 
		SELECT 
			unsub.drupal_uid,
			unsub.email,
			unsub.confirm_time,
			'customer_unsubscribed' AS event_type
		FROM mailchimp_final_exports.final_mailchimp_main_unsub unsub
	UNION ALL
		SELECT 
			iunsub.drupal_uid,
			iunsub.email,
			iunsub.confirm_time,
			'customer_unsubscribed' AS event_type
		FROM mailchimp_final_exports.final_mailchimp_intl_unsub iunsub
	) mc
INNER JOIN northstar.users_mysql u ON u.drupal_uid::varchar = mc.drupal_uid::varchar
--WHERE u.drupal_uid <> 0
);


SELECT count(*)
FROM northstar.users_mysql u 
LEFT JOIN mc_import c ON u.northstar_id = c.customer_id 
WHERE u.customer_io_subscription_timestamp IS NULL 
AND u.customer_io_subscription_status IS NOT NULL AND c."timestamp" IS NULL ;


DROP TABLE IF EXISTS public.legacy_cio_status_import;
CREATE TABLE public.legacy_cio_status_import AS 
	(
	SELECT DISTINCT *
	FROM 
		(	SELECT 
				'' AS email_id,
				u.northstar_id AS customer_id, 
				u.email AS email_address, 
				NULL AS template_id,
				MD5(CONCAT(u.northstar_id, u.email, u.customer_io_subscription_timestamp)) AS event_id,
				u.customer_io_subscription_timestamp AS "timestamp", 
				CASE 
					WHEN u.customer_io_subscription_status = 'subscribed' THEN 'customer_subscribed'
					ELSE 'customer_unsubscribed' END AS event_type
			FROM northstar.users_mysql u 
			WHERE u.customer_io_subscription_status IS NOT NULL 
		UNION ALL 
			SELECT *
			FROM mc_import mc
		) stat
	WHERE stat."timestamp" IS NOT NULL 
	)
;

ALTER TABLE public.legacy_cio_status_import ADD PRIMARY KEY (event_id, customer_id, "timestamp", event_type);
CREATE INDEX leg_cio_indices ON public.legacy_cio_status_import (event_id, customer_id, "timestamp", event_type);

SELECT 
	use.customer_io_subscription_status,
	count(*)
FROM northstar.users_mysql use
LEFT JOIN 
	(SELECT 
		u.northstar_id,
		max(COALESCE(u.customer_io_subscription_timestamp, i."timestamp")) AS cust_ts
	FROM northstar.users_mysql u
	INNER JOIN public.legacy_cio_status_import i ON i.customer_id = u.northstar_id
	GROUP BY u.northstar_id) max_ts 
	ON max_ts.northstar_id = use.northstar_id
WHERE use.customer_io_subscription_status IS NOT NULL 
AND max_ts.cust_ts IS NULL 
GROUP BY use.customer_io_subscription_status
;

