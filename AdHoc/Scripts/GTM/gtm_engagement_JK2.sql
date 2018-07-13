-- Confirm "grab the mic" campaign ID (campaign_node_id=8017, campaign_run_id=8022)
select * 
from public.campaign_info
where campaign_node_id_title ilike '%mic%'
	

-- 1. (DONE) GTM members who signed up in January and have completed at least one action in another GTM month
	--filter GTM campaign only 
	--filter those who signed up in January
	--filter those post_created_at has a value and in a different month from signup_created_at 
	--filter out duplicate northstar_id's
	
select 
	distinct c.northstar_id 
from public.campaign_activity c
where c.campaign_run_id=8022 
	and (c.signup_created_at between '2018-01-01' and '2018-01-31') 
	and (c.post_created_at is not null and c.post_created_at >= '2018-02-01') 


-- 2. (DONE) GTM members who have completed 2+ months of the campaign (but did NOT sign up in January) 
	--filter GTM campaign only 
	--filter those who signed up AFTER January 
	--filter those whose count of post_created >= 2
	--filter out duplicate northstar_id's

select
	distinct c.northstar_id
from public.campaign_activity c
where c.campaign_run_id = 8022 
	and c.signup_created_at >= '2018-02-01' 
	and c.post_id is not null 
group by c.northstar_id
having count(distinct extract('month' from c.post_created_at)) >= 2


-- 3. (UPDATED in R) GTM members who have gone on to complete an action on another campaign 		
	-- select GTM members 
	-- join with itself only those who ALSO have a DIFFERENT campaign_run_id (done in R)
	-- filter those whose campaign_run_id=8022 & sign_up_created is BEFORE campaign_run_id <>  8022 & post_created_at (done in R)


select northstar_id, 
	campaign_run_id,
	signup_created_at,
	post_created_at
from campaign_activity
where campaign_run_id = 8022 and post_created_at is not null

select northstar_id,
	campaign_run_id,
	signup_created_at,
	post_created_at
from campaign_activity
where campaign_run_id <> 8022 and post_created_at is not null


-- 4. (DONE) GTM members who signed up and have not completed an action
select 
	distinct c.northstar_id
from public.campaign_activity c
where c.campaign_run_id=8022 
	and c.post_id is null


-- 5. (DONE) GTM members who have completed only one action 
select 
	distinct c.northstar_id 
from public.campaign_activity c
where c.campaign_run_id=8022
group by c.northstar_id
having count(c.post_id)=1


-- 6. (UPDATED) How many members have engaged more than one month in GTM? (198 members)

select 
	count(*)
from 
	(select 
		c.northstar_id
	from campaign_activity c
	where c.campaign_run_id = 8022
		and c.post_created_at is not null 	
	group by c.northstar_id
	having count(distinct extract('month' from post_created_at)) > 1) a 


-- 7. (UPDATED) How many members see the pitch (main) page of GTM (avg)? (88,865)
	-- James (puck data: website data) 
	-- Phoenix_events (column name ref or URL grab the mic) 
	
select 
	count(distinct c.northstar_id)
from phoenix_events c 
where c.campaign_name = 'grab-mic'
	and (c.event_name = 'visit' or c.event_name = 'view') 
	and c.northstar_id is not null

-- 8. (UPDATED in R) Avg time GTM members spend on site.
	-- what is the pageview table? 
	-- log in / logout time (average): table: users_log 
	-- create data that includes landing & end timestamps 
	-- create data with GTM member info 
	-- calculate average time spent 

SELECT
    e.northstar_id,
    e.session_id,
    max(to_timestamp(e.ts/1000)) AS ending_ts,
    min(to_timestamp(s.landing_ts/1000)) AS landing_ts
FROM phoenix_events e
LEFT JOIN phoenix_sessions s ON s.session_id = e.session_id
WHERE e.northstar_id IS NOT NULL
        AND e.northstar_id <> ''
GROUP BY e.northstar_id, e.session_id

select 
	distinct c.northstar_id
from campaign_activity c
where c.campaign_run_id=8022


-- 9. (UPDATED - same query but number increased by 9, worked in SQL) 
-- How many members who signed up in January, re-engaged during another month? (62,590)
	--isolate those who signed up through GTM in January 
	--left join with member_event_log 
	--join on northstar_id and events after February (subquery: distinct northstar_ids)

select 
	count(distinct c.northstar_id)
from public.campaign_activity c 
left join member_event_log u on c.northstar_id = u.northstar_id
where c.campaign_run_id=8022 and 
	c.signup_created_at between '2018-01-01' and '2018-01-31' and
	u."timestamp" >= '2018-02-01' AND 
	u.action_type <> 'account_creation'


-- 10. (UPDATED in R) How many members have taken 2+ actions?
	--filter those whose count of post_ids > 2 (in R: 987 out of 15122 have taken 2 or more actions)

select 
	c.northstar_id,
	c.post_id,
	c.post_type,
	c.post_action
from public.campaign_activity c
where c.campaign_run_id=8022 


-- 11. (UPDATED in R) What actions are members taking the most often?

--by action type: 
-- 1. march-2018-turbovote      7075
-- 2. default                   4277
-- 4 april-2018-turbovote       4101
-- 5 february-2018-turbovote    1015
-- 6 may-2018-turbovote          425
-- 7 january-2018-turbovote       54
-- 8 june-2018-turbovote          32
-- 9 september-2017-turbovote      1

--by post type:
--1. voter-reg  12703
--2. photo       4277

-- 12. Actions & posts by month (in SQL and R)

-- by action type 

SELECT  
	date_trunc('month', c.post_created_at) AS dates, 
    c.post_action,
    count(*)
FROM public.campaign_activity c
WHERE c.campaign_run_id=8022 
    AND c.post_action IS NOT NULL
GROUP BY c.post_action,
     date_trunc('month', c.post_created_at)
ORDER BY dates

-- by post type 

SELECT 
	date_trunc('month', c.post_created_at) AS dates,
	c.post_type,
	count(*)
FROM public.campaign_activity c
WHERE c.campaign_run_id = 8022
	AND c.post_type IS NOT NULL
GROUP BY c.post_type,
	date_trunc('month', c.post_created_at)
ORDER BY dates 


-- 13. How many GTM members have engaged in 2+ months within GTM OR any other campaign? 
	-- filter GTM signups 
	-- left join with member_events (timestamp, action_type)
	-- remove account_creation  
	-- filter so only include members whose u.timestamp is AFTER c.signup_created_at 
	-- having month count >= 2
	-- (125,264 members) 

SELECT 
	count(DISTINCT c.northstar_id)
FROM campaign_activity c 
LEFT JOIN member_event_log u 
	ON u.northstar_id = c.northstar_id
WHERE c.campaign_run_id = 8022 
	AND u."timestamp" >= c.signup_created_at 
	AND u.action_type <> 'account_creation' -- we should include this, yes? 
HAVING count(DISTINCT date_trunc('month', u.timestamp)) >= 2 -- we need to include distinct, yes? 


-- 14. How many GTM members have completed more than one action (in more than one month), (regardless of when they signed up)?
	-- EXCLUDES those who have taken more than one action but only in the same month
-- (15,147 members)

SELECT 
	count(distinct c.northstar_id)
FROM campaign_activity c
WHERE c.campaign_run_id = 8022 
	AND c.post_created_at IS NOT NULL
HAVING count(DISTINCT c.post_created_at) >= 4 -- 3 means 1 action in 1 month and 2 in another. 4 means 2 in 1 month and 2 in another. 
	AND count(DISTINCT date_trunc('month', c.post_created_at)) >= 2
	


-- 15. How many members have taken an action in one month of GTM AND 
-- taken an action in at least one other month of GTM? 
-- (basically it would be a CSV of all members who participated in 2+ months of GTM)
	-- GTM members 
	-- months in post_created_at is greater than 2 
	-- (15,147 members): not sure why #14 and #15 are the same.... 
	
SELECT 
	count(DISTINCT c.northstar_id)
FROM campaign_activity c
WHERE c.campaign_run_id = 8022 
	AND c.post_created_at IS NOT NULL 
HAVING count(DISTINCT date_trunc('month', c.post_created_at)) >= 2


-- 16. How many members have gone on to complete an action on another campaign (NOT GTM)? 

-- (1560) in R, adapted from question 3  







	



	