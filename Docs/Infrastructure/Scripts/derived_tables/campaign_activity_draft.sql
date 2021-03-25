DROP MATERIALIZED VIEW IF EXISTS public.signups_test CASCADE;
CREATE MATERIALIZED VIEW public.signups_test AS 
    (SELECT 
        sd.northstar_id AS northstar_id,
        sd.id AS id,
        sd.campaign_id AS campaign_id,
        sd.campaign_run_id AS campaign_run_id,
        sd.why_participated AS why_participated,
        sd."source" AS "source",
        sd.created_at AS created_at
    FROM 
        (SELECT 
        		stemp.id,
        		max(stemp.updated_at) AS updated_at
        FROM rogue.signups stemp
        WHERE stemp.deleted_at IS NULL
        AND stemp."source" IS DISTINCT FROM 'runscope'
        AND stemp."source" IS DISTINCT FROM 'runscope-oauth'
        AND stemp.why_participated IS DISTINCT FROM 'why_participated_ghost_test'
        GROUP BY stemp.id) s_maxupt
        INNER JOIN rogue.signups sd
            ON sd.id = s_maxupt.id AND sd.updated_at = s_maxupt.updated_at
    )
    ; 
CREATE INDEX signups_testi ON public.signups_test (id, created_at); 

DROP MATERIALIZED VIEW IF EXISTS public.latest_post_test CASCADE;
CREATE MATERIALIZED VIEW public.latest_post_test AS
    (SELECT 
        pd.id AS id,
        pd."type" AS "type",
        pd."action" AS "action",
        pd.status AS status,
        pd.quantity AS quantity,
        pd."source" AS "source",
        pd.created_at AS created_at,
        pd.url AS url,
        pd.signup_id AS signup_id
    FROM 
        (SELECT 
            ptemp.id,
            max(ptemp.updated_at) AS updated_at
         FROM rogue.posts ptemp
         WHERE ptemp.deleted_at IS NULL
         AND ptemp."source" IS DISTINCT FROM 'runscope'
         AND ptemp."source" IS DISTINCT FROM 'runscope-oauth'
        GROUP BY ptemp.id) p_maxupt
     INNER JOIN rogue.posts pd
            ON pd.id = p_maxupt.id AND pd.updated_at = p_maxupt.updated_at  
    )
    ;
CREATE INDEX latest_post_testi ON public.latest_post_test (id, created_at); 

DROP MATERIALIZED VIEW IF EXISTS public.posts_test CASCADE;
CREATE MATERIALIZED VIEW public.posts_test AS 
    (SELECT 
            pd.id AS id,
            pd."type" AS "type",
            pd."action" AS "action",
            pd.status AS status,
            pd.quantity AS quantity,
            pd."source" AS "source",
            COALESCE(tv.created_at, pd.created_at) AS created_at,
            pd.url AS url,
            pd.signup_id AS signup_id
    FROM public.latest_post_test pd
    LEFT JOIN rogue.turbovote tv ON tv.post_id::bigint = pd.id)
;
CREATE INDEX post_testi ON public.posts_test (id, created_at); 

DROP MATERIALIZED VIEW IF EXISTS public.reported_back_test CASCADE;
CREATE MATERIALIZED VIEW public.reported_back_test AS 
    (SELECT 
        temp_posts.signup_id,
        MAX(CASE WHEN temp_posts.id <> -1 THEN 1 ELSE 0 END) AS reported_back
    FROM 
        public.posts_test temp_posts
    WHERE temp_posts.signup_id IS NOT NULL
    GROUP BY 
        temp_posts.signup_id
    ) 
    ; 
CREATE INDEX reported_back_testi ON public.reported_back_test (signup_id);

DROP MATERIALIZED VIEW IF EXISTS public.campaign_activity_testing;
CREATE MATERIALIZED VIEW public.campaign_activity_testing AS 
    (
    SELECT 
    		ca.*,
    		min(ca.post_created_at) OVER (PARTITION BY ca.signup_id, ca.post_class) AS post_attribution_date
    FROM 
	    (SELECT  
	        a.northstar_id AS northstar_id,
	        a.id AS signup_id,
	        b.id AS post_id,
	        a.campaign_id AS campaign_id,
	        a.campaign_run_id AS campaign_run_id,
	        b."type" AS post_type,
	        b."action" AS post_action,
	        CASE WHEN a.campaign_id = '822' AND a.created_at >= '2018-05-01' 
	        		 THEN 'voter-reg - ground'
	        		 ELSE CONCAT(b."type", ' - ', b."action") END AS post_class,
	        	CASE WHEN b.id IS NULL THEN NULL
	        		 WHEN a.campaign_id = '822' AND a.created_at >= '2018-05-01' AND b.status = 'accepted' THEN b.quantity
	        		 ELSE 1 END AS reportback_volume,
	        b.status AS post_status,
	        a.why_participated AS why_participated,
	        b.quantity AS quantity,
	        a."source" AS signup_source,
	        b."source" AS post_source,
	        a.created_at AS signup_created_at,
	        b.created_at AS post_created_at,
	        c.reported_back AS reported_back,
	        b.url AS url
	    FROM 
	        public.signups_test a
	    LEFT JOIN 
	        public.posts_test b
	        ON b.signup_id = a.id
	    LEFT JOIN 
	        public.reported_back_test c
	        ON c.signup_id = a.id
	    ) ca
	 )
    ;
CREATE INDEX ON public.campaign_activity_testing (northstar_id, signup_id, post_id, post_created_at, post_attribution_date);
GRANT SELECT ON public.campaign_activity_testing TO looker;
GRANT SELECT ON public.campaign_activity_testing TO jjensen;
GRANT SELECT ON public.campaign_activity_testing TO jli;
GRANT SELECT ON public.campaign_activity_testing TO shasan;