SELECT 
	gtmfb."month", 
	gtmfb.url,
	count(*)
FROM 
	(SELECT 
		substring(e.event_datetime::varchar,1,7) AS "month",
		regexp_replace(e."path", '/us/campaigns/', '') AS url
	FROM public.phoenix_events e 
	WHERE e.event_name IN ('share action completed','facebook share posted')
		AND e.event_datetime >= '2018-01-01'
		AND e.campaign_id = '8017'
		AND e."path" NOT LIKE '%gtm-test%'
	) gtmfb
GROUP BY gtmfb."month", gtmfb.url
;