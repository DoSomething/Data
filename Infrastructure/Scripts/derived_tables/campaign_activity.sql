DROP MATERIALIZED VIEW IF EXISTS public.signups CASCADE;
CREATE MATERIALIZED VIEW public.signups AS 
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
	    GROUP BY stemp.id) 
	    s_maxupt
		INNER JOIN rogue.signups sd
			ON sd.id = s_maxupt.id AND sd.updated_at = s_maxupt.updated_at
	)
	; 
CREATE INDEX signup_source ON public.signups (id, created_at); 
DROP MATERIALIZED VIEW IF EXISTS public.posts CASCADE;
CREATE MATERIALIZED VIEW public.posts AS
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
	FROM 
		(SELECT 
	        ptemp.id,
	        max(ptemp.updated_at) AS updated_at
     	FROM rogue.posts ptemp
     	WHERE ptemp.deleted_at IS NULL
     	AND ptemp."source" IS DISTINCT FROM 'runscope'
     	AND ptemp."source" IS DISTINCT FROM 'runscope-oauth'
        GROUP BY ptemp.id) 
	    p_maxupt
	    INNER JOIN rogue.posts pd
			ON pd.id = p_maxupt.id AND pd.updated_at = p_maxupt.updated_at  
	LEFT JOIN rogue.turbovote tv ON tv.post_id::bigint = pd.id
	)
	;
CREATE INDEX post_index ON public.posts (id, created_at); 
DROP MATERIALIZED VIEW IF EXISTS public.reported_back CASCADE;
CREATE MATERIALIZED VIEW public.reported_back AS 
	(SELECT 
		temp_posts.signup_id,
		MAX(CASE WHEN temp_posts.id <> -1 then 1 else 0 end) AS reported_back
	FROM 
		rogue.posts temp_posts
	WHERE temp_posts.signup_id IS NOT NULL
	GROUP BY 
		temp_posts.signup_id
	) 
	; 
CREATE INDEX reported_back_index ON public.reported_back (signup_id);
DROP MATERIALIZED VIEW IF EXISTS public.campaign_activity;
CREATE MATERIALIZED VIEW public.campaign_activity AS 
	(SELECT  
		a.northstar_id AS northstar_id,
		a.id AS signup_id,
		b.id AS post_id,
		a.campaign_id AS campaign_id,
		a.campaign_run_id AS campaign_run_id,
		b.type AS post_type,
		b.status AS post_status,
		a.why_participated AS why_participated,
		b.quantity AS quantity,
		a.source AS signup_source,
		b.source AS post_source,
		a.created_at AS signup_created_at,
		b.created_at AS post_created_at,
		c.reported_back AS reported_back,
		b.url AS url
	FROM 
		public.signups a
	LEFT JOIN 
		public.posts b
		ON b.signup_id = a.id
	LEFT JOIN 
		public.reported_back c
		ON c.signup_id = a.id
	)
	;
CREATE INDEX ON public.campaign_activity (northstar_id, signup_id, post_id, post_created_at);
GRANT SELECT ON public.campaign_activity TO looker;
GRANT SELECT ON public.campaign_activity TO jjensen;
GRANT SELECT ON public.campaign_activity TO jli;
GRANT SELECT ON public.campaign_activity TO shasan;