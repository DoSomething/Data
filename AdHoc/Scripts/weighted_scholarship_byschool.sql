SELECT setseed(0.75);
SELECT *
FROM (
    SELECT
	s.id AS signup_id,
	s.campaign_id,
	s.created_at AS signup_created_at,
	s.northstar_id,
	u.first_name,
	u.email,
	u.mobile,
	u.school_id,
	COALESCE(weights.weight, 1)::float AS weight
    FROM signups s
    LEFT JOIN users u ON s.northstar_id = u.northstar_id
    JOIN (
	SELECT
	    school_id,
	    CASE
		WHEN count(*) > 99 THEN 3
		WHEN count(*) > 50 THEN 2
		WHEN count(*) > 10 THEN 1.5
		ELSE 1 END AS weight
	FROM (
	    SELECT s.northstar_id, u.school_id
	    FROM signups s
	    LEFT JOIN users u ON u.northstar_id = s.northstar_id
	    WHERE campaign_id = '9037'
	    AND u.school_id IS NOT NULL
	) f
	GROUP BY school_id
    ) weights ON u.school_id = weights.school_id
    WHERE campaign_id = '9037'
    AND s.created_at >= '2019-12-01' and s.created_at < '2019-12-21'
) f2
ORDER BY -log(random())/weight ASC
LIMIT 30;
