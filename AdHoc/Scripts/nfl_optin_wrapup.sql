SELECT
	DISTINCT *
FROM
	(
	SELECT
		NULL AS "Unique ID",
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
		END AS "First Name",
		CASE
			WHEN s.details ILIKE '%keyword%' THEN NULL
			WHEN s.details ILIKE '%broadcast%' THEN NULL
			WHEN (s.details::JSONB) ->> 'affiliateOptIn' IS NULL THEN NULL
			ELSE u.last_name
		END AS "Last Name",
		NULL AS "Address1",
		NULL AS "Address2",
		NULL AS "City",
		NULL AS "State",
		NULL AS "Country",
		u.zipcode AS "Zip",
		CASE 
			WHEN s.details ILIKE '%keyword%'THEN NULL
			WHEN s.details ILIKE '%broadcast%' THEN NULL
			WHEN (s.details::JSONB) ->> 'affiliateOptIn' = 'true' THEN 'Y'
		END AS "NFL Reg Optin2",
		NULL AS "NFL Reg Optin3",
		'NULL' AS "Site Code",
		s.created_at AS "Date Created"
	FROM
		signups s
	LEFT JOIN users u ON
		s.northstar_id = u.northstar_id
	WHERE
		s.campaign_id = '9011'
		AND u.email NOT ILIKE '%dosomething.org%' 
	) nfl
    WHERE nfl."NFL Reg Optin2" = 'Y'
;
