SELECT email, country_code
FROM final_mailchimp_intl_sub
WHERE 
country_code IS NOT NULL
AND country_code <> '';

SELECT u.email, u.country 
FROM quasar.users u
WHERE u.country IS NOT NULL
AND u.country <> 'US'
AND u.email IS NOT NULL
;

SELECT DISTINCT u.email, u.country_code 
FROM voting.users u 
WHERE u.`country_code` IS NOT NULL
AND u.`country_code` <> 'US';

SELECT 
ca.signup_id,
ca.post_id,
max(CASE WHEN ca.status LIKE 'rejected' THEN 1 ELSE 0 END) AS ever_rejected
FROM quasar.campaign_activity ca 
WHERE ca.campaign_run_id = 7772
GROUP BY ca.signup_id, ca.post_id;




