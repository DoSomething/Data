-- trello request from Anthony A. https://trello.com/c/r4N6teqw 
-- analysis goal to understand historical effectiveness of drip messaging 
-- method: track percentage of users who complete a post after receiving drip messaging 


-- create temp table to link broadcast_id (provided by Anthony) to campaign_id (manually searched for in public.campaign_info by Kat).
-- this is necessary because the campaign_id field in public.messages_flattened is mostly null and not always accurate when popluated

--	create table analyst_sandbox.kk_broadcast_campaign 
--		(ID serial primary key,
--		broadcast_id varchar(255),
--		campaign_id varchar(255)
--		);
--		
--	insert into analyst_sandbox.kk_broadcast_campaign (ID, broadcast_id, campaign_id)
--	values
--		(1,'55vG6qkqnC6QGAeCEeuQYI',8142),
--		(2,'4agMVK0DkkkkAOUmqweYGG',8142),
--		(3,'6yYe4o80128cIKyOQ2EqYk',8142),
--		(4,'2cy1orWGTWAWI0CyEoMGkm',8226),
--		(5,'2Us4Or19zyi0kWYkGQeUcc',8226),
--		(6,'3CxWxRHrIsg8WuuumUwAQ',8226),
--		(7,'1A9XR09vA8E4GGugIIACQe',5646),
--		(8,'MIPHOR4w4mWeEGOCsAc4g',5646),
--		(9,'4wVv8J3fioaEmMCsyEyw4C',5646),
--		(10,'7CuQQtIBIAASGoqWImW6UE',9000),
--		(11,'Mb8Ne6MrimwMom060c6Sw',9000),
--		(12,'1xD41yAQy8CWsISW4kkoCu',9000),
--		(13,'5UmgBzYvBKkciuWyQiyWGc',8308),
--		(14,'5iMCHXFyEMu0SckASUSCOm',8308),
--		(15,'1A3veedW8ISgmMmgoIiuAe',8308),
--		(16,'1nkpa8NOo7sXEgNgGzZqyn',9003),
--		(17,'1wvycjlONuzVn0IzeHrvPD',9034),
--		(18,'6DHN4lNbrI1hFmWXuolLzS',9045),
--		(19,'7Mzm3cKyL8zDIfxfAz4qnK',9069),
--		(20,'1fHvHB2iRLkRLSlAeJMJZP',9073),
--		(21,'FHwEQ9LLQqFekcMC2YWwl',9077),
--		(22,'6Q0SzI0e01dlXIFLDtiwaz',9109),
--		(23,'24VlxoLr8N5RfKZds1IjuJ',9037),
--		(24,'tYuAHZ6zFcsWPAxotbv9G',8292),
--		(25,'4Os6MyyToDmGnHcmvMZhg0',8303),
--		(26,'19GgZu2WocN821DceztxXn',9011),
--		(27,'702SBK6pUNjiSiRMXuKaXd',9018),
--		(28,'3dWzOy0xIsIh4m1UQroBGG',9003),
--		(29,'1P9elRgCfjsqWlTNDvUSTg',9025),
--		(30,'7EbAwj8QqAJ7qEex9rasBg',9024),
--		(31,'5fAUdkJHHZH9yfloB41Lyq',9045),
--		(32,'1nUdetwJu3gmnGc428OZH1',9109),
--		(33,'74vDYpoK5uj2IpHhdRkInM',9120),
--		(34,'6nMfiClVZxleTqvHMg7a7w',9109),
--		(35,'4OD9FQae7uV0nxBB3n2ndj',9037),
--		(36,'2XpYrGJh2oycMME2mmWEOq',8142)
--	;
	

-- join broadcast_id and campaign_id to all users who received drip
with 
broadcast_users as 
	(select 
		bc.broadcast_id,
		bc.campaign_id,
		mf.user_id as northstar_id,
		date(mf.created_at) as drip_created_at
	from 
		analyst_sandbox.kk_broadcast_campaign bc
		left join public.messages_flattened mf
		using (broadcast_id)
	)
	
-- find users first post per campaign
, first_post as 
	(select 
		bu.campaign_id,
		bu.northstar_id,
		min(date(p.created_at)) as post_created_at
	from 
		broadcast_users bu
		left join posts p
		using (campaign_id, northstar_id)
	group by 
		1, 2
	)
	
-- join all users who received drip and also submitted a post
, user_posts as 
	(select 
		bu.*,
		fp.post_created_at
	from 
		broadcast_users bu
		left join first_post fp
		using (campaign_id, northstar_id)
	)
	
-- join date of campaign
, campaign_date as 
	(select  
		up.*
	,	date(ci.campaign_run_start_date) as campaign_run_start_date
	from 
		user_posts up
		left join campaign_info ci
		on cast(up.campaign_id as int) = ci.campaign_id
	)
	
-- calculate time between drip message and post
, time_to_post as 
	(select
		cd.*,
		case 
			when post_created_at-drip_created_at = 0 
			then 1 else 0
		end as same_day,
		case 
			when post_created_at-drip_created_at = 1 
			then 1 else 0
		end as second_day,
		case 
			when post_created_at-drip_created_at = 2 
			then 1 else 0
		end as third_day,
		case 
			when post_created_at-drip_created_at > 2 
			then 1 else 0
		end as after_third_day
	from 
		campaign_date cd
	)


-- count total users who received drip and users who submitted a post
, total_drip_users as 
	(select 
		campaign_id 
	,	campaign_run_start_date
	,	count(distinct northstar_id)::float as drip_users
	,	count(post_created_at)::float as drip_post_users
	,	sum(same_day)::float as total_same_day
	,	sum(second_day)::float as total_second_day
	,	sum(third_day)::float as total_third_day
	,	sum(after_third_day)::float as total_after_third_day
	from 
		time_to_post ttp
	group by 
		campaign_id
	,	campaign_run_start_date
	order by 
		campaign_run_start_date
	)
	
-- 	find total signups and unique post users per campaign
, find_total_users as 
	(select 
		tdp.campaign_id
	,	count(distinct s.northstar_id)::float as total_signups
	,	count(distinct p.northstar_id)::float as total_post_users
	from 
		total_drip_users tdp
		left join signups s 
		using (campaign_id)
		left join posts p
		using (campaign_id)
	group by 
		1
	)
	
-- add total signups and unique post users per campaign
, total_users as 
	(select 
		tdp.*
	,	ftu.total_signups
	,	ftu.total_post_users
	from 
		total_drip_users tdp
		left join find_total_users ftu
		using (campaign_id)
	)


-- calculate percentage of drip users who complete a post
select 
	campaign_id
,	campaign_run_start_date
,	total_signups
,	total_post_users
,	drip_users
,	drip_post_users
,	round((total_post_users/total_signups)*100, 2) as total_post_percent
,	round((drip_post_users/drip_users)*100, 2) as drip_post_percent
,	case
		when drip_post_users > 0 
		then round((total_same_day/drip_post_users)*100, 2)
		else 0 
	end as same_day_percent
,	case 
		when drip_post_users > 0 
		then round((total_second_day/drip_post_users)*100, 2)
		else 0 
	end as second_day_percent
,	case 
		when drip_post_users > 0 
		then round((total_third_day/drip_post_users)*100, 2)
		else 0 
	end as third_day_percent
,	case 
		when drip_post_users > 0 
		then round((total_after_third_day/drip_post_users)*100, 2)
		else 0 
	end as after_third_day_percent
from 
	total_users tu
	

