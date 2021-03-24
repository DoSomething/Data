SELECT 
	reports.northstar_id,
	sum(reports.reportback) AS reportbacks_across_campaigns
FROM
	(SELECT 
		s.northstar_id, 
		s.created_at,
		s.campaign_run_id , 
		CASE WHEN count(p.id) > 0 THEN 1 ELSE 0 END AS reportback
	FROM signups s
	LEFT JOIN posts p ON s.id = p.signup_id
	GROUP BY s.id)  reports
WHERE reports.created_at >= '2013-01-01'
GROUP BY reports.northstar_id
HAVING sum(reports.reportback) > 1
ORDER BY sum(reports.reportback) DESC
;
