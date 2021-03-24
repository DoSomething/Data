SELECT
   MAX(CASE WHEN GREATEST(u.last_accessed, u.last_logged_in) <= u.created_at 
     AND u.source='niche' 
     AND COUNT(DISTINCT c.signup_id) <= 1
     AND COUNT(DISTINCT c.post_id) <= 1
  THEN 1 ELSE 0 END) as bad_niche
FROM quasar.users u
LEFT JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
GROUP BY u.northstar_id