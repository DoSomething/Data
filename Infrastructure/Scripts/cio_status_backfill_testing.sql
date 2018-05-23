DROP TABLE IF EXISTS mailchimp;
CREATE TEMPORARY TABLE mailchimp AS 
(	SELECT 
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
	) 
;

DROP TABLE IF EXISTS mc_import;
CREATE TEMPORARY TABLE mc_import AS 
	(SELECT 
		'' AS email_id,
		u.northstar_id AS customer_id,
		mc.email AS email_address,
		NULL AS template_id,
		MD5(CONCAT(u.northstar_id, mc.email, mc.confirm_time)) AS event_id,
		CASE WHEN mc.event_type = 'customer_subscribed' THEN COALESCE(mc.confirm_time, mdru.confirm_time) 
		ELSE COALESCE(mc.confirm_time, mdru.confirm_time) END AS "timestamp",
		COALESCE(mc.event_type, mdru.event_type) AS event_type
	FROM northstar.users_mysql u
	LEFT JOIN mailchimp mc ON u.email = mc.email
	LEFT JOIN 
		(SELECT 
			dru.drupal_uid,
			dru.event_type,
			max(dru.confirm_time) AS confirm_time
		FROM mailchimp dru
		WHERE dru.drupal_uid <> '0'
		GROUP BY dru.drupal_uid, dru.event_type) mdru ON mdru.drupal_uid::varchar = u.drupal_uid::varchar
	WHERE u.customer_io_subscription_status IS NOT NULL 
	AND (mc.email IS NOT NULL OR mdru.drupal_uid IS NOT NULL)
	)
;

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

insert into cio.customer_event 
select email_id::varchar(96), customer_id::varchar(64), email_address::varchar(255),
template_id::int4, event_id::varchar(64), "timestamp", event_type::varchar(64)
from public.legacy_cio_status_import on conflict do nothing;

DROP TABLE IF EXISTS public.cio_status_no_ts_best_guess;
CREATE TABLE public.cio_status_no_ts_best_guess AS 
	(SELECT 
		'' AS email_id,
		u.northstar_id AS customer_id,
		u.email AS email_address,
		NULL AS template_id,
		MD5(CONCAT(u.northstar_id, u.email, u.created_at)) AS event_id,
		CASE WHEN u.customer_io_subscription_status = 'subscribed' THEN u.created_at
		ELSE COALESCE(u.last_logged_in, u.created_at ) END AS "timestamp",
		CASE WHEN u.customer_io_subscription_status = 'subscribed' THEN 'customer_subscribed' 
		ELSE 'customer_unsubscribed' END AS event_type
	FROM northstar.users_mysql u
	LEFT JOIN public.cio_latest_status c ON u.northstar_id = c.customer_id
	WHERE u.customer_io_subscription_status IS NOT NULL 
	AND c.customer_id IS NULL 
	)
	;

insert into cio.customer_event 
select email_id::varchar(96), customer_id::varchar(64), email_address::varchar(255),
template_id::int4, event_id::varchar(64), "timestamp", event_type::varchar(64)
from public.cio_status_no_ts_best_guess on conflict do nothing;