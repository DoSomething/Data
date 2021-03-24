SELECT 
	(regexp_split_to_array(c.target_url, '='))[3] AS broadcast,
	count(*)
FROM public.clicks c
WHERE c.target_url ILIKE '%broadcastid%'
GROUP BY (regexp_split_to_array(c.target_url, '='))[3]