SELECT * FROM 
    (SELECT 
        c.signup_id, c.campaign_run_id,
        max(CASE WHEN c.status = 'accepted' THEN 1 ELSE 0 end) AS any_accepted,
        max(CASE WHEN c.status = 'rejected' THEN 1 ELSE 0 end) AS any_rejected
    FROM quasar.campaign_activity c
    GROUP BY c.signup_id) a
WHERE a.any_accepted=1 AND a.any_rejected=1
LIMIT 1000