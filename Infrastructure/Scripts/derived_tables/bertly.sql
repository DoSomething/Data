SELECT 
	*,
	(regexp_split_to_array(
		regexp_split_to_array(c.target_url, 'user'))[2]
		)[2] AS t2
FROM bertly.clicks c