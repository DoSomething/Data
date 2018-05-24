DROP MATERIALIZED VIEW IF EXISTS public.users;
DROP MATERIALIZED VIEW IF EXISTS public.cio_latest_status;

CREATE MATERIALIZED VIEW public.cio_latest_status AS 
	(SELECT 
		cio.customer_id,
		max(CASE WHEN 
				cio.event_type = 'customer_unsubscribed' 
				THEN 'customer_unsubscribed' 
				ELSE 'customer_subscribed' END) AS event_type,
		max(cio."timestamp") AS "timestamp"
	FROM cio.customer_event cio
	INNER JOIN 
		(SELECT 
			ctemp.customer_id,
			max(ctemp."timestamp") AS max_update
		FROM cio.customer_event ctemp
		GROUP BY ctemp.customer_id) cio_max 
			ON cio_max.customer_id = cio.customer_id 
			AND cio_max.max_update = cio."timestamp"
	GROUP BY cio.customer_id
	)
;
		
CREATE INDEX cio_indices ON public.cio_latest_status (customer_id);

CREATE MATERIALIZED VIEW public.users AS 
	(SELECT 
		u.id AS northstar_id,
		u.created_at,
		umax.max_last_auth AS last_logged_in,
		umax.max_last_access AS last_accessed,
		u.drupal_id AS drupal_uid,
		u."source",
		u.email,
		u.facebook_id,
		u.mobile,
		CASE WHEN 
			u.birthdate < '1900-01-01' OR 
			u.birthdate > (date('now') - INTERVAL '10 years') 
			THEN NULL ELSE u.birthdate END AS birthdate,
		u.first_name,
		u.last_name,
		u.addr_street1 AS address_street_1,
		u.addr_street2 AS address_street_2,
		u.addr_city AS city,
		u.addr_state AS state,
		u.addr_zip AS zipcode,
		u.country,
		u."language",
		email_status.event_type AS cio_status,
		email_status."timestamp" AS cio_status_timestamp,
		u.sms_status,
		u.source_detail,
		CASE WHEN 
			u.sms_status in ('active','less') OR 
			email_status.event_type = 'customer_subscribed' 
			THEN TRUE ELSE FALSE END AS subscribed_member,
		umax.max_update AS last_updated_at
	FROM northstar.users u
	INNER JOIN 
		(SELECT
			utemp.id,
			max(utemp.updated_at) AS max_update,
			max(utemp.last_accessed_at) AS max_last_access,
			max(utemp.last_authenticated_at) AS max_last_auth
		FROM northstar.users utemp
		GROUP BY utemp.id) umax ON umax.id = u.id AND umax.max_update = u.updated_at
	LEFT JOIN public.cio_latest_status email_status ON email_status.customer_id = u.id
	WHERE u."source" IS DISTINCT FROM 'runscope'
	AND u."source" IS DISTINCT FROM 'runscope-client'
	AND u.email IS DISTINCT FROM 'runscope-scheduled-test@dosomething.org'
	AND u.email IS DISTINCT FROM 'juy+runscopescheduledtests@dosomething.org'
	AND (u.email NOT ILIKE '%@example.org%' OR u.email IS NULL) 
	)
	;

CREATE INDEX du_indices 
	ON public.users (northstar_id, created_at, email, mobile, "source");

GRANT SELECT ON public.users TO jjensen;
GRANT SELECT ON public.users TO public;
GRANT SELECT ON public.users TO looker;
GRANT SELECT ON public.users TO shasan;
GRANT SELECT ON public.users TO jli;