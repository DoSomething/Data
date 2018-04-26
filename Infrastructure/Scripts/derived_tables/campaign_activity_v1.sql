SELECT  
	a.northstar_id AS northstar_id,
	a.id AS signup_id,
	b.id AS post_id,
	b.quantity AS quantity,
	a.campaign_id AS campaign_id,
	a.source AS signup_source,
	b.source AS post_source,
	a.created_at AS signup_created_at,
	b.created_at AS post_created_at,
	c.reported_back AS reported_back,
FROM 
	(SELECT *
	FROM 
		rogue.signups s
	WHERE 
		s.updated_at = MAX(updated_at)
	GROUP BY 
		s.id
	) a
LEFT JOIN 
	(SELECT *
	FROM 
		rogue.posts p
	WHERE 
		p.updated_at = MAX(p.updated_at)
	GROUP BY 
		p.id
	) b
	ON b.signup_id = a.id
LEFT JOIN 
	(SELECT 
		temp_posts.signup_id,
		MAX(CASE WHEN temp_posts.post_id <> -1 then 1 else 0 end) AS reported_back
	FROM 
		rogue.posts temp_posts
	GROUP BY 
		temp_posts.signup_id
	) c
	ON c.signup_id = a.id
WHERE s.SOURCE IS DISTINCT FROM 'runscope'
AND s.SOURCE IS DISTINCT FROM 'runscope-oauth'
AND s.deleted_at IS NULL

	
	