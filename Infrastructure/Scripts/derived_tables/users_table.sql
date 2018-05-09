DROP MATERIALIZED VIEW IF EXISTS public.derived_user_test;
CREATE MATERIALIZED VIEW public.derived_user_test AS 
	(SELECT 
		u.*,
		email_status.event_type AS cio_subs_status
	FROM northstar.users u
	INNER JOIN 
		(SELECT
			utemp.id,
			max(utemp.updated_at) AS max_update
		FROM northstar.users utemp
		GROUP BY utemp.id) umax ON umax.id = u.id AND umax.max_update = u.updated_at
	LEFT JOIN 
		(
		SELECT 
			cio.customer_id,
			cio.event_type
		FROM cio.customer_event cio
		INNER JOIN  
			(SELECT 
				ctemp.customer_id,
				max(ctemp."timestamp") AS max_update
			FROM cio.customer_event ctemp
			GROUP BY ctemp.customer_id) cio_max ON cio_max.customer_id = cio.customer_id AND cio_max.max_update = cio."timestamp"
		) email_status ON email_status.customer_id = u.id 
	)
	;

CREATE INDEX dut_indices ON derived_user_test (northstar_id, created_at, updated_at);

GRANT SELECT ON public.derived_user_test TO jjensen;
GRANT SELECT ON public.derived_user_test TO public;
GRANT SELECT ON public.derived_user_test TO looker;
GRANT SELECT ON public.derived_user_test TO shasan;
GRANT SELECT ON public.derived_user_test TO jli;
