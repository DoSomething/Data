SELECT 
	substring(c."timestamp"::varchar,1, 7)::varchar AS year_month,
	c.campaign_id,
	camp.name AS campaign,
	count(*)
FROM calls c
LEFT JOIN campaign_campaign camp ON c.campaign_id = camp.id
WHERE status = 'completed'
GROUP BY substring(c."timestamp"::varchar, 1, 7), c.campaign_id, camp.name
ORDER BY substring(c."timestamp"::varchar, 1, 7);