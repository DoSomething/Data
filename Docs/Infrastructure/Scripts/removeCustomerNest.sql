SELECT 
	*,
	c.event #- '{data,variables,customer}'
FROM cio.event_log c
INNER JOIN eu_users.northstar_id eu
	ON eu.id = c.event #>> '{data,customer_id}';
