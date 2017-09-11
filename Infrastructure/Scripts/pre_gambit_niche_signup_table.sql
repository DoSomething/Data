select 
	l.ms_phone_number,
	l.mc_run_id,
	qu.northstar_id,
	l.mu_uid,
	qu.drupal_uid,
	niche_flags.number_is_niche
from mobile_master_lookup_lite l
left join 
	(select 
		m.ms_phone_number,
		m.mc_run_id,
		max(case when m.ms_opt_in_id IN (210839, 206777, 215113,216832, 207601, 213831) then 1 else 0 end) as number_is_niche
	from 
		users_and_activities.mobile_master_lookup_lite m
	group by 
		m.ms_phone_number, m.mc_run_id) niche_flags 
	ON 
		niche_flags.ms_phone_number = l.ms_phone_number and niche_flags.mc_run_id = l.mc_run_id 
left join quasar.users qu
	ON qu.drupal_uid = l.mu_uid
where niche_flags.number_is_niche = 1
and l.ms_opt_in_id = 206777
;; 


select 
	a.signup_source as 'signup_source'
	a.signup_created_at as 'signup_created_at'



	from(
		 select ### campaign sign up ###
                ca.northstar_id as 'northstar_id',
                ca.signup_created_at as 'timestamp',
                "sign-up" as 'action',
                "1" as 'action_id',
                ca.signup_source as 'source',
                ca.signup_id as 'action_serial_id'
              from
                quasar.campaign_activity ca
              where ca.signup_created_at > '0000-00-00 00:00:00'
            union all
              select ### campaign reportback ###
                rb.northstar_id as 'northstar_id',
                rb.submission_created_at as 'timestamp',
                "reportback" as 'action',
                "2" as 'action_id',
                rb.post_source as 'source',
                rb.post_id as 'action_serial_id'
              from
                quasar.campaign_activity rb
                where rb.post_id > 0