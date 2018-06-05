CREATE MATERIALIZED VIEW public.bertly_clicks AS (
	SELECT 
		*,
		(regexp_split_to_array(
			(regexp_split_to_array(
				(regexp_split_to_array(
					c.target_url, 'user')
				)[2], E'[=:]+')
			)[2], 
			E'[^a-zA-Z0-9]')
		)[1] AS northstar_id
	FROM bertly.clicks c
);