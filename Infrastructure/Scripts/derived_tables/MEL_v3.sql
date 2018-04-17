SELECT
    MD5(concat(a.northstar_id, a.timestamp, a.action_id, a.action_serial_id)) AS event_id,
    a.northstar_id AS northstar_id,
    a.timestamp AS timestamp,
    a.action AS action_type,
    a.action_id AS action_id,
   	a.source AS source
FROM ( 
	SELECT -- CAMPAIGN SIGNUP
		DISTINCT s.northstar_id AS northstar_id,
		s.created_at AS timestamp,
		'signup' AS action,
		'1' AS action_id, 
		s.source AS source,
		s.id AS action_serial_id 
	FROM 
		rogue.signups s
	WHERE s.deleted_at IS NULL
	UNION ALL 
	SELECT -- CAMPAIGN POSTS
		DISTINCT p.northstar_id AS northstar_id,
		p.created_at AS timestamp,
		'post' AS action,
		'2' AS action_id,
		p.source AS source,
		p.id AS action_serial_id	
	FROM 
		rogue.posts p
	WHERE p.deleted_at IS NULL
	UNION ALL
	SELECT -- SITE ACCESS
		DISTINCT u.id AS northstar_id,
		u.last_accessed_at AS timestamp,
		'site_access' AS action,
		'3' AS action_id,
		NULL AS source,
		'0' AS action_serial_id
	FROM
		northstar.users u
	UNION ALL 
	SELECT -- SITE LOGIN
		DISTINCT u.id AS northstar_id,
		u.last_authenticated_at AS timestamp,
		'site_login' AS action,
		'4' AS action_id,
		NULL AS source,
		'0' AS action_serial_id
	FROM 
		northstar.users u
	UNION ALL 
	SELECT -- ACCOUNT CREATION 
		DISTINCT u.id AS northstar_id,
		u.created_at AS timestamp,
		'account_creation' AS action, 
		'5' AS action_id,
		u.source AS source,
		u.id AS action_serial_id
	FROM
		northstar.users u
	UNION ALL 
	SELECT -- LAST MESSAGED SMS 
		DISTINCT u.id AS northstar_id,
		u.last_messaged_at AS timestamp,
		'messaged_gambit' AS action, 
		'6' AS action_id,
		'SMS' AS source,
		'0' AS action_serial_id
	FROM
		northstar.users u
	UNION ALL 
	SELECT -- CLICKED EMAIL LINK 
		DISTINCT cio.customer_id AS northstar_id,
		cio.timestamp AS timestamp,
		'clicked_link' AS action,
		'7' AS action_id,
		cio.template_id::CHARACTER AS source,
		'0' AS action_serial_id
	FROM
		cio.email_event cio
	WHERE 
		cio.event_type = 'email_clicked'
	) AS a 
WHERE a.action = 'clicked_link'
ORDER BY timestamp DESC 
LIMIT 1000