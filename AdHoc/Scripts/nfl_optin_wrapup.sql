SELECT
	DISTINCT *
FROM
	(
	SELECT
		CASE
			WHEN s.details ILIKE '%keyword%' THEN u.northstar_id || '@dosomething.org'
			WHEN s.details ILIKE '%broadcast%' THEN u.northstar_id || '@dosomething.org'
			WHEN (s.details::JSONB) ->> 'affiliateOptIn' IS NULL THEN u.northstar_id || '@dosomething.org'
			ELSE u.email
		END AS "Email",
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
			WHEN s.details ILIKE '%keyword%'THEN NULL
			WHEN s.details ILIKE '%broadcast%' THEN NULL
			ELSE (s.details::JSONB) ->> 'affiliateOptIn' 
		END AS "NFL Reg Optin",
		NULL AS "Site Code",
		u.zipcode AS "Zip Code",
		s.created_at AS "Created At"
	FROM
		signups s
	LEFT JOIN users u ON
		s.northstar_id = u.northstar_id
	WHERE
		s.campaign_id = '9011'
		AND u.email NOT ILIKE '%dosomething.org%' 
	) nfl
    WHERE nfl."NFL Reg Optin" = 'true'
;
