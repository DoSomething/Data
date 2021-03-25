SELECT 
	u.northstar_id, 
	u.country  AS country,
	COUNT(DISTINCT u.northstar_id ) AS `u.count_distinct_northstar_id`,
	extract(year FROM (from_days(dateDiff(current_timestamp,u.birthdate)))) AS age
FROM quasar.users as u

WHERE 
	(u.country  IN ('CA', 'GB', 'AU', 'MX', 'JP'))
GROUP BY 1
ORDER BY COUNT(DISTINCT u.northstar_id ) DESC