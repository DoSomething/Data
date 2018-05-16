DROP TABLE IF EXISTS public.users_log_to_users_test;
DROP TABLE IF EXISTS users_log_to_users_test_dup;

CREATE TEMPORARY TABLE users_log_to_users_test_dup as
	(SELECT DISTINCT 
		ul.northstar_id AS id,
		ul.first_name,
		ul.last_name,
		'' AS last_initial,
		NULL AS photo,
		ul.email,
		ul.mobile,
		ul.facebook_id,
		NULL AS interests,
		ul.birthdate,
		ul.addr_street1,
		ul.addr_street2,
		ul.addr_city,
		ul.addr_state,
		ul.addr_zip,
		NULL AS addr_source,
		ul."source",
		ul.source_detail,
		NULL AS slack_id,
		ul.sms_status,
		false AS sms_paused,
		ul."language",
		ul.country,
		ul.drupal_uid,
		NULL AS "role",
		CASE WHEN 
			ul.last_accessed IS NOT NULL 
			THEN ul.last_accessed 
			ELSE ul.last_logged_in END AS last_accessed_at,
		ul.last_logged_in AS last_authenticated_at,
		NULL AS last_messaged_at,
		GREATEST(ul.last_accessed, ul.last_logged_in, ul.created_at) AS updated_at,
		ul.created_at AS created_at
	FROM northstar.users_log_mysql ul
	WHERE 
		(ul.last_accessed IS NOT NULL OR 
		ul.last_logged_in IS NOT NULL) AND
		(ul.last_accessed + INTERVAL '5h' <> '1970-01-01 00:00:00' OR
		ul.last_logged_in + INTERVAL '5h' <> '1970-01-01 00:00:00')
	)
;

CREATE TABLE public.users_log_to_users_test AS 
	(SELECT 
		d.id, 
		d.created_at,
		d.updated_at,
		max(d.first_name) AS first_name,
		max(d.last_name) AS last_name,
		max(d.last_initial) AS last_initial,
		max(photo) AS photo,
		max(d.email) AS email,
		max(d.mobile) AS mobile,
		max(d.facebook_id) AS facebook_id,
		max(d.interests) AS interests,
		max(d.birthdate) AS birthdate,
		max(d.addr_street1) AS addr_street1,
		max(d.addr_street2) AS addr_street2,
		max(d.addr_city) AS addr_city,
		max(d.addr_state) AS addr_state,
		max(d.addr_zip) AS addr_zip,
		max(d.addr_source) AS addr_source,
		max(d."source") AS "source",
		max(d.source_detail) AS source_detail,
		max(d.slack_id) AS slack_id,
		max(d.sms_status) AS sms_status,
		max(d.sms_paused::varchar) AS sms_paused,
		max(d."language") AS "language",
		max(d.country) AS country,
		max(d.drupal_uid) AS drupal_uid,
		max("role") AS "role",
		max(d.last_accessed_at) AS last_accessed_at,
		max(d.last_authenticated_at) AS last_authenticated_at,
		max(last_messaged_at) AS last_messaged_at
	FROM users_log_to_users_test_dup d
	GROUP BY d.id, d.created_at, d.updated_at)
;

ALTER TABLE public.users_log_to_users_test ADD PRIMARY KEY (id, created_at, updated_at);
CREATE INDEX nult_indices ON public.users_log_to_users_test (id, created_at, updated_at);

GRANT SELECT ON public.users_log_to_users_test TO jjensen;
GRANT SELECT ON public.users_log_to_users_test TO looker;