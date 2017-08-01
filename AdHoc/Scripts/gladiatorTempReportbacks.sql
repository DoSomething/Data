SELECT s.northstar_id AS "Northstar ID", s.campaign_run_id , count(p.id) AS "number_of_posts"
FROM signups s
LEFT JOIN posts p ON s.id = p.signup_id
WHERE s.campaign_run_id IN (7811, 7827, 7877, 7825, 7873)
GROUP BY s.id
;