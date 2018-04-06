SELECT 
	session_times.northstar_id,
	sum(session_times.session_length) AS time_on_site
FROM 
	(SELECT
		e.northstarid_s AS northstar_id,
		e.session_id,
		max(to_timestamp(e.ts/1000)) - min(to_timestamp(s.landing_ts/1000)) AS session_length
	FROM phoenix_next_events e
	LEFT JOIN phoenix_next_sessions s ON s.session_id = e.session_id
	WHERE e.northstarid_s IS NOT NULL 
		AND e.northstarid_s <> ''
		AND to_timestamp(e.ts/1000) >= '2018-01-01'
	GROUP BY e.northstarid_s, e.session_id) session_times
GROUP BY session_times.northstar_id