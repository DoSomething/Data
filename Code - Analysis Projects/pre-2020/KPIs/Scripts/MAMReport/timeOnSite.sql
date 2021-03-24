SELECT
	e.northstar_id,
	e.session_id,
	max(to_timestamp(e.ts/1000)) AS ending_ts,
	min(to_timestamp(s.landing_ts/1000)) AS landing_ts
FROM phoenix_events e
LEFT JOIN phoenix_sessions s ON s.session_id = e.session_id
WHERE e.northstar_id IS NOT NULL
		AND e.northstar_id <> ''
GROUP BY e.northstar_id, e.session_id
;