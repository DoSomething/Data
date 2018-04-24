SELECT
	s.northstar_id AS northstar_id,
	s.id AS signup_id,
	p.id AS post_id,
	p.quantity AS quantity,
	s.campaign_id AS campaign_id,
	s."source" AS signup_source,
	p.SOURCE AS post_source,
	s.created_at AS signup_created_at,
	p.created_at AS post_created_at
FROM 
	rogue.signups s
LEFT JOIN 
	rogue.posts p
	ON p.signup_id = s.id AND p.id = s.post_id
GROUP BY 1,2,3,4,5,6,7,8,9
	