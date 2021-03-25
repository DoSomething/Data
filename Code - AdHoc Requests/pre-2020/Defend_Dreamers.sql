/*For https://trello.com/c/n4qfXkbr/1138-defend-dreamers-week-of-action-analysis request*/
/*Identify members who participated in Defend Dreamers: pull the number of campaigns, how long they've been a member, their age, and mobile to match on with MoCo data*/
Select distinct ca.northstar_id, 
count(DISTINCT ca.signup_id) AS total_signups, 
Year(now()) - YEAR(u.northstar_created_at_timestamp) as 'years_a_member',
max(CASE WHEN ca.campaign_run_id IN (7928) THEN 1 ELSE 0 END) AS participated_in_dd, 
u.birthdate,
u.mobile as 'handset_number'
From quasar.campaign_activity ca
Left join quasar.users u 
on ca.northstar_id=u.northstar_id
Group by ca.northstar_id
