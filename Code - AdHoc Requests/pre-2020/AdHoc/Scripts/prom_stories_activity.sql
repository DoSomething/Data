SELECT
	actions.which_action,
	COUNT(*) total_actions,
	COUNT(DISTINCT actions.device) AS distinct_devices
FROM
	(
		SELECT 
			e."event" #>> '{name}' event_name,
			CASE
				WHEN e."event" #>> '{name}' = 'waypoint reached' THEN e."data" #>> '{waypointName}'
				ELSE 'page_view'
			END AS which_action,
			e."user" #>> '{deviceId}' AS device
		FROM
			ft_puck_heroku_wzsf6b3z.events e
		WHERE
			(e.event #>> '{name}' ILIKE '%waypoint%'
			OR e."event" #>> '{name}' = 'view')
			AND e.page #>> '{path}' ILIKE '%prom%'
			AND e.page #>> '{path}' ILIKE '%stories%'
	) actions
GROUP BY
	actions.which_action