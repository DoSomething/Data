
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