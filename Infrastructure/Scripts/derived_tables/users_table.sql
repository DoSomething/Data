DROP MATERIALIZED VIEW IF EXISTS public.derived_user_test;
DROP MATERIALIZED VIEW IF EXISTS public.cio_latest_status;

CREATE MATERIALIZED VIEW public.cio_latest_status AS 
	(SELECT 
			cio.customer_id,
			cio.event_type,
			cio."timestamp"
		FROM cio.customer_event cio
		INNER JOIN 
			(SELECT 
				ctemp.customer_id,
				max(ctemp."timestamp") AS max_update
			FROM cio.customer_event ctemp
			GROUP BY ctemp.customer_id) cio_max ON cio_max.customer_id = cio.customer_id AND cio_max.max_update = cio."timestamp"
		)
;
		
CREATE INDEX cio_indices ON public.cio_latest_status (customer_id);

CREATE MATERIALIZED VIEW public.derived_user_test AS 
	(SELECT 
		u.id AS northstar_id,
		u.created_at,
		u.last_authenticated_at AS last_logged_in,
		u.last_accessed_at AS last_accessed,
		u.drupal_id AS drupal_uid,
		u."source",
		u.email,
		u.facebook_id,
		u.mobile,
		u.birthdate,
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
		u.source_detail
	FROM northstar.users u
	INNER JOIN 
		(SELECT
			utemp.id,
			max(utemp.updated_at) AS max_update
		FROM northstar.users utemp
		GROUP BY utemp.id) umax ON umax.id = u.id AND umax.max_update = u.updated_at
	LEFT JOIN public.cio_latest_status email_status ON email_status.customer_id = u.id
	)
	;

CREATE INDEX dut_indices ON public.derived_user_test (northstar_id, created_at, email, mobile, "source");

GRANT SELECT ON public.derived_user_test TO jjensen;
GRANT SELECT ON public.derived_user_test TO public;
GRANT SELECT ON public.derived_user_test TO looker;
GRANT SELECT ON public.derived_user_test TO shasan;
GRANT SELECT ON public.derived_user_test TO jli;