### Full campaign activity 

(select distinct 
		qu.northstar_id as `northstar_id`,
		mml.mc_run_id as 'campaign_run_id',
		'sms_only' as 'signup_source',
		min(mml.ms_activated_at) as 'signup_created_at'
from users_and_activities.mobile_master_lookup_lite mml
left join dosomething.dosomething_signup ds
	on mml.mu_uid = ds.uid and mml.mc_run_id = ds.run_nid
left join quasar.users qu
	on qu.drupal_uid = mml.mu_uid
where ds.uid IS NULL
and mml.mu_uid IS NOT NULL
group by mml.mu_uid, mml.mc_run_id

UNION
SELECT  
		ca.northstar_id as `northstar_id`, 
		ca.campaign_run_id as `campaign_run_id`,
		ca.signup_source as `signup_source`,
		ca.signup_created_at as `signup_created_at`
FROM quasar.campaign_activity ca) full_act