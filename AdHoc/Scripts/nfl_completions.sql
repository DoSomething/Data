SELECT
	DISTINCT *
FROM
	(
	SELECT s.created_at AS "timestamp",
		CASE
			WHEN s.details ILIKE '%keyword%' THEN NULL
			WHEN s.details ILIKE '%broadcast%' THEN NULL
			WHEN (s.details::JSONB) ->> 'affiliateOptIn' IS NULL THEN NULL
			ELSE u.first_name
		END AS first_name,
		CASE
			WHEN s.details ILIKE '%keyword%' THEN NULL
			WHEN s.details ILIKE '%broadcast%' THEN NULL
			WHEN (s.details::JSONB) ->> 'affiliateOptIn' IS NULL THEN NULL
			ELSE u.last_name
		END AS last_name,
		CASE
			WHEN s.details ILIKE '%keyword%' THEN u.northstar_id || '@dosomething.org'
			WHEN s.details ILIKE '%broadcast%' THEN u.northstar_id || '@dosomething.org'
			WHEN (s.details::JSONB) ->> 'affiliateOptIn' IS NULL THEN u.northstar_id || '@dosomething.org'
			ELSE u.email
		END AS email,
		u.zipcode AS "zip",
		CASE
			WHEN u.source_detail ILIKE '%utm_medium%' THEN (STRING_TO_ARRAY((STRING_TO_ARRAY(u.source_detail, ','))[1], ':'))[2]
			ELSE NULL
		END AS utm_medium,
		CASE 
			WHEN s.details ILIKE '%keyword%'THEN NULL
			WHEN s.details ILIKE '%broadcast%' THEN NULL
			ELSE (s.details::JSONB) ->> 'affiliateOptIn' 
		END AS comms_opt_in,
		i.campaign_name,
		CASE
			WHEN p.action_id = 892 THEN p."text"
			ELSE NULL
		END AS team
	FROM
		signups s
	INNER JOIN posts p ON p.signup_id = s.id
	LEFT JOIN users u ON
		s.northstar_id = u.northstar_id
	LEFT JOIN campaign_info i ON
		s.campaign_id::BIGINT = i.campaign_id
	WHERE
		s.campaign_id = '9011'
		AND u.email NOT ILIKE '%dosomething.org%' 
		AND s.created_at >= (current_date - cast(abs(extract(dow from current_date) - 7) + 2 as int))
		AND s.created_at < (current_date - cast(abs(extract(dow from current_date) - 7) - 5 as int))
	) nfl
    WHERE nfl.comms_opt_in = 'true'
;
