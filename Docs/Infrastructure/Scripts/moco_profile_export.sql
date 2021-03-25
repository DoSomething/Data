SELECT 
	u.northstar_id,
	UNIX_TIMESTAMP(u.northstar_created_at_timestamp) AS created_at,
	DATE(u.birthdate) AS birthdate,
	u.first_name,
	u.last_name,
	u.`language`,
	m.mobile,
	m.status AS sms_status,
	u.northstar_id_source_name AS source,
	COALESCE(NULLIF(u.addr_street1, ''), NULLIF(m.addr_street1, '')) AS addr_street1,
	COALESCE(NULLIF(u.addr_street2, ''), NULLIF(m.addr_street2, '')) AS addr_street2,
	COALESCE(NULLIF(u.addr_city,''), NULLIF(m.addr_city,''), NULLIF(m.loc_city,'')) AS addr_city,
	COALESCE(NULLIF(u.addr_state,''), NULLIF(m.addr_state,''), NULLIF(m.loc_state, '')) AS addr_state,
	COALESCE(NULLIF(u.addr_zip, ''), NULLIF(m.addr_zip, ''), NULLIF(m.loc_zip,'')) AS addr_zip,
	COALESCE(NULLIF(u.country, ''), NULLIF(m.addr_country,''), NULLIF(m.loc_country,'')) AS addr_country
FROM quasar.moco_profile_import m
LEFT JOIN quasar.users u 
	ON m.moco_id = u.moco_commons_profile_id
WHERE u.moco_commons_profile_id IS NOT NULL;