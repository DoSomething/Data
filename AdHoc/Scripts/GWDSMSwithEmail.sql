SELECT DISTINCT
    u.northstar_id,
    u.mobile,
    u.email
FROM quasar.users u
LEFT JOIN quasar.campaign_activity c on c.northstar_id = u.northstar_id
WHERE c.campaign_id = 5651
AND u.email is not null
AND c.signup_source NOT LIKE '%sms%'
;

select distinct c.signup_source from quasar.campaign_activity c