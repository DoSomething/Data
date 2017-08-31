
select distinct
    ca.northstar_id,
    ca.post_id,
    ca.submission_created_at,
    ca.campaign_run_id,
    qu.email,
    qu.first_name
from quasar.campaign_activity ca
left join quasar.users qu on qu.northstar_id = ca.northstar_id
where ca.campaign_run_id = 7897
and ca.post_id IS NOT NULL
and ca.submission_created_at >= TIMESTAMP('2017-08-20 12:00:00')
and ca.submission_created_at <= TIMESTAMP('2017-08-23 15:00:00')
group by ca.northstar_id
limit 50
;

SELECT
    u.northstar_id,
    extract(year FROM (from_days(dateDiff(current_timestamp,u.birthdate)))) AS age,
    u.email,
    u.mobile
FROM quasar.users u
WHERE
(u.email is not null OR u.mobile is not null)
AND extract(year FROM (from_days(dateDiff(current_timestamp,u.birthdate)))) > 18
limit 50
;