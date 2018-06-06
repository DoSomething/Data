DROP MATERIALIZED VIEW IF EXISTS public.member_event_log; 
CREATE MATERIALIZED VIEW public.member_event_log AS 
    (SELECT
        MD5(concat(a.northstar_id, a."timestamp", a.action_id, a.action_serial_id)) AS event_id,
        a.northstar_id AS northstar_id,
        a."timestamp" AS "timestamp",
        a."action" AS action_type,
        a.action_id AS action_id,
        a."source" AS "source",
        a.action_serial_id AS action_serial_id
    FROM ( 
        SELECT -- CAMPAIGN SIGNUP
            DISTINCT s.northstar_id AS northstar_id,
            s.created_at AS "timestamp",
            'signup' AS "action",
            '1' AS action_id, 
            s."source" AS "source",
            s.id::varchar AS action_serial_id 
        FROM 
             (
            SELECT 
				sd.northstar_id,
				sd.created_at,
				sd.id,
				sd."source",
				sd.deleted_at
            FROM 
				(SELECT 
                    stemp.id,
                    max(stemp.updated_at) AS updated_at
  				FROM rogue.signups stemp
				GROUP BY stemp.id) s_maxupt
            INNER JOIN rogue.signups sd
                ON sd.id = s_maxupt.id AND sd.updated_at = s_maxupt.updated_at
            ) s
        WHERE s.deleted_at IS NULL
	UNION ALL 
        SELECT -- CAMPAIGN POSTS
            DISTINCT p.northstar_id AS northstar_id,
            p.created_at AS "timestamp",
            'post' AS "action",
            '2' AS action_id,
            p."source" AS "source",
            p.id::varchar AS action_serial_id   
        FROM 
            (
            SELECT 
                pd.northstar_id,
                pd.created_at,
                pd.id,
                pd."source",
                pd.deleted_at,
                pd."type"
            FROM 
				(SELECT 
                    ptemp.id,
                    max(ptemp.updated_at) AS updated_at
				FROM rogue.posts ptemp
				GROUP BY ptemp.id) p_maxupt
			INNER JOIN rogue.posts pd
            ON pd.id = p_maxupt.id AND pd.updated_at = p_maxupt.updated_at
                ) p
        WHERE p.deleted_at IS NULL
        AND p."type" IS DISTINCT FROM 'voter-reg'
	UNION ALL -- SITE ACCESS
        SELECT DISTINCT 
            u_access.id AS northstar_id,
            u_access.last_accessed_at AS "timestamp",
            'site_access' AS "action",
            '3' AS action_id,
            NULL AS "source",
            '0' AS action_serial_id
        FROM northstar.users u_access
		WHERE u_access.last_accessed_at IS NOT NULL
	UNION ALL -- SITE LOGIN
        SELECT DISTINCT 
            u_login.id AS northstar_id,
            u_login.last_authenticated_at AS "timestamp",
            'site_login' AS "action",
            '4' AS action_id,
            NULL AS "source",
            '0' AS action_serial_id
        FROM northstar.users u_login
        WHERE u_login.last_authenticated_at IS NOT NULL 
	UNION ALL 
        SELECT -- ACCOUNT CREATION 
            DISTINCT u.id AS northstar_id,
            u.created_at AS "timestamp",
            'account_creation' AS action, 
            '5' AS action_id,
            u."source" AS "source",
            '0' AS action_serial_id
        FROM
            (SELECT 
            		u_create.id,
            		max(u_create."source") AS "source",
            		min(u_create.created_at) AS created_at
            FROM northstar.users u_create
            GROUP BY u_create.id) u
	UNION ALL 
        SELECT -- LAST MESSAGED SMS 
            DISTINCT u.id AS northstar_id,
            u.last_messaged_at AS "timestamp",
            'messaged_gambit' AS "action", 
            '6' AS action_id,
            'SMS' AS "source",
            '0' AS action_serial_id
        FROM
            northstar.users u
        WHERE u.last_messaged_at IS NOT NULL 
	UNION ALL 
        SELECT -- CLICKED EMAIL LINK 
            DISTINCT cio.customer_id AS northstar_id,
            cio."timestamp" AS "timestamp",
            'clicked_link' AS "action",
            '7' AS action_id,
            cio.template_id::CHARACTER AS "source",
            cio.event_id AS action_serial_id
        FROM
            cio.email_event cio
        WHERE 
            cio.event_type = 'email_clicked'
	UNION ALL
        SELECT -- VOTER REGISTRATIONS
            DISTINCT tv.nsid AS northstar_id,
            tv.created_at AS "timestamp",
            'registered' AS "action",
            '8' AS action_id,
            tv.source_details AS "source",
            tv.id as action_serial_id
        FROM
            public.turbovote_file tv
        WHERE
            tv.ds_vr_status IN ('register-form', 'confirmed', 'register-OVR')
        AND 
            tv.nsid IS NOT NULL AND tv.nsid <> ''
	UNION ALL 
        SELECT -- FACEBOOK SHARES FROM PHOENIX-NEXT
            DISTINCT pe.northstar_id AS northstar_id,
            to_timestamp(pe.ts /1000) AS "timestamp",
            'facebook share completed' AS "action",
            '9' AS action_id,
            pe.event_source AS "source",
            pe.event_id AS action_serial_id
        FROM 
            public.phoenix_events pe
        WHERE 
            pe.event_name IN ('share action completed', 'facebook share posted')
        AND pe.northstar_id IS NOT NULL
        AND pe.northstar_id <> ''
	UNION ALL 
    		SELECT -- SMS LINK CLICKS FROM BERTLY
    			b.northstar_id AS northstar_id,
    			b.click_time AS "timestamp",
    			'sms_link_click' AS "action",
    			'10' AS action_id,
    			'bertly' AS "source",
    			b.click_id AS action_serial_id
    		FROM public.bertly_clicks b	
    		WHERE b.northstar_id IS NOT NULL 
		) AS a 
	); 

 CREATE INDEX ON public.member_event_log (event_id, northstar_id, "timestamp", action_serial_id);

GRANT SELECT ON public.member_event_log TO looker;
GRANT SELECT ON public.member_event_log TO jjensen;
GRANT SELECT ON public.member_event_log TO jli;
GRANT SELECT ON public.member_event_log TO shasan;