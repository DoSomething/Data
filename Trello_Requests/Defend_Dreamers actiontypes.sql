/*For https://trello.com/c/n4qfXkbr/1138-defend-dreamers-week-of-action-analysis request*/
/*Identify members who participated in Defend Dreamers - the campaigns they've signed up for, cause types, action types, mobile number to match on. Don't pull distinct NSIDS because need to total the number of action types and cause types they've signed up for in the past*/

Select ca.northstar_id,  
ca.campaign_run_id,
ci.campaign_cause_type as 'cause_type',
ci.campaign_action_type as'action_type',
max(CASE WHEN ca.campaign_run_id IN (7928) THEN 1 ELSE 0 END) AS participated_in_dd, 
u.mobile as 'handset_number'
From quasar.campaign_activity ca
Left join quasar.users u 
on ca.northstar_id=u.northstar_id
Left join quasar.campaign_info ci
on ca.campaign_run_id = ci.campaign_run_id
Group by ca.northstar_id