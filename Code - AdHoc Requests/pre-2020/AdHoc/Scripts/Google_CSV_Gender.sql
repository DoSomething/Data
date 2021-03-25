/*Ban Binaries*/
select distinct ca.signup_id, ci.campaign_node_id_title, ci.campaign_run_id, qu.first_name, qu.birthdate, ca.why_participated, ca.url from quasar.campaign_activity ca left join campaign_info ci on ci.campaign_run_id = ca.campaign_run_id left join quasar.users qu on qu.northstar_id = ca.northstar_id where ci.campaign_run_id IN ('6240') and ca.why_participated <> ''

/*I am a Woman, Hear me Score*/
select distinct ca.signup_id, ci.campaign_node_id_title, ci.campaign_run_id, qu.first_name, qu.birthdate, ca.why_participated, ca.url from quasar.campaign_activity ca left join campaign_info ci on ci.campaign_run_id = ca.campaign_run_id left join quasar.users qu on qu.northstar_id = ca.northstar_id where ci.campaign_run_id IN ('7173', '7062') and ca.why_participated <> ''

/*Girls Only Edit-A-Thon*/
select distinct ca.signup_id, ci.campaign_node_id_title, ci.campaign_run_id, qu.first_name, qu.birthdate, ca.why_participated, ca.url from quasar.campaign_activity ca left join campaign_info ci on ci.campaign_run_id = ca.campaign_run_id left join quasar.users qu on qu.northstar_id = ca.northstar_id where ci.campaign_run_id IN ('6260') and ca.why_participated <> ''

/*Reelwomen*/
select distinct ca.signup_id, ci.campaign_node_id_title, ci.campaign_run_id, qu.first_name, qu.birthdate, ca.why_participated, ca.url from quasar.campaign_activity ca left join campaign_info ci on ci.campaign_run_id = ca.campaign_run_id left join quasar.users qu on qu.northstar_id = ca.northstar_id where ci.campaign_run_id IN ('6596') and ca.why_participated <> ''

/*Real Princess*/
select distinct ca.signup_id, ci.campaign_node_id_title, ci.campaign_run_id, qu.first_name, qu.birthdate, ca.why_participated, ca.url from quasar.campaign_activity ca left join campaign_info ci on ci.campaign_run_id = ca.campaign_run_id left join quasar.users qu on qu.northstar_id = ca.northstar_id where ci.campaign_run_id IN ('6208') and ca.why_participated <> ''

/*Read Outside the Lines*/
select distinct ca.signup_id, ci.campaign_node_id_title, ci.campaign_run_id, qu.first_name, qu.birthdate, ca.why_participated, ca.url from quasar.campaign_activity ca left join campaign_info ci on ci.campaign_run_id = ca.campaign_run_id left join quasar.users qu on qu.northstar_id = ca.northstar_id where ci.campaign_run_id IN ('6444') and ca.why_participated <> ''

