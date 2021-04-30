-- trello request from Anthony A. https://trello.com/c/r4N6teqw 
-- analysis goal to understand historical effectiveness of drip messaging 
-- method: track percentage of users who complete a post after receiving drip messaging 


-- create temp table to link cio_campaign_name (from cio_email_events) to campaign_id (manually searched for in public.campaign_info by Kat).
-- this is necessary because the cio_campaign_id field in public.cio_email_events is not equivalent to our internal campaign_id
-- the list of cio_campaign_name values to use was determined by searching for cio_campaign_type = 'behavioral', then manually removing emails related to
-- VR, badges, NPS, and other non-drip 

--	create table analyst_sandbox.kk_email_drip_campaigns 
--		(ID serial primary key,
--		cio_campaign_name varchar(255),
--		campaign_id varchar(255)
--		);
--		
--	insert into analyst_sandbox.kk_email_drip_campaigns (ID, cio_campaign_name, campaign_id)
--	values
--		(1,'CancelBullying2019_Signup_ReportbackReceived_ReportbackApproved',9026),
--		(2,'CelebratePride2020_Signup',9073),
--		(3,'Feeding Better Futures re-engagment',7984),
--		(4,'GiveMasGetMas2019_Signup_LMSDrip_ReportbackReceived_ReportbackApproved',9036),
--		(5,'GiveMasGetMas2019_Signup_ReportbackReceived_ReportbackApproved',9036),
--		(6,'GoThereGreener2020_Signup',9109),
--		(7,'HuddleForHeroes2019_Signup_ReportbackReceived_ReportbackApproved',9034),
--		(8,'HuddleUpToBrightenSomeonesDay2019_Signup_ReportbackReceived_ReportbackApproved',9030),
--		(9,'HuddleUpToPlayItForward2019_Signup',9024),
--		(10,'Impact_FiveActionsChallenge_SignupDrip',8303),
--		(11,'IMPACT - Ride & Seek Sweepstakes',9018),
--		(12,'IMPACT - Thanks a Billion Sweepstakes',7992),
--		(13,'IMPACT_TreatYoFriends_SignupAndDrip',5646),
--		(14,'InvestInUs2020_Signup',9028),
--		(15,'InvestInUs2020_TextRBReceived',9028),
--		(16,'MentalNote2020_Signup',9045),
--		(17,'MentalNote2020_Signup&Drip',9045),
--		(18,'NFL_HuddleAgainstHunger_Signup&Reportback',9011),
--		(19,'OhThePlacesWe''llGo2020_Signup',9069),
--		(20,'OneTeamChallenge2020_Signup',9065),
--		(21,'RedefineBlack_TriggeredSignup',9016),
--		(22,'SeniorHomies2020_Signup',9064),
--		(23,'ShredHate2019_Signup_ReportbackReceived_ReportbackApproved',9025),
--		(24,'SincerelyUs2019_Signup&Drip_CampaignFlow',9020),
--		(25,'TeensForJeans2020_DripMessage',9037),
--		(26,'TheHitWellTake2019_Signup_ReportbackReceived_ReportbackApproved',9031),
--		(27,'UntangleTheWeb2019_Signup&Drip_CampaignFlow',9003),
--		(28,'WholeHistories2021_CompletedCampaign_Interests',9115),
--		(29,'WouldYouRather2020_RBReceived',9052),
--		(30,'WouldYouRather2020_Signup',9052)
--	;


-- join campaign_id to email event info
with 
drip_emails as 
	(select 
		kedc.cio_campaign_name 
	,	kedc.campaign_id 
	,	cee.customer_id as northstar_id
	,	cee.event_type 
	,	max(date(cee."timestamp")) as event_date 
	from 
		analyst_sandbox.kk_email_drip_campaigns kedc 
		left join cio_email_events cee 
		using (cio_campaign_name)
	group by 
		1, 2, 3, 4
	)

-- restructure data to have most recent drip date and link clicked date per user and campaign
, email_sent_date as
	(select 
		campaign_id
	,	northstar_id
	,	max(event_date) as drip_date
	from 
		drip_emails de
	where 
		event_type = 'email_sent'
	group by 
		1,2
	)	
	
, email_clicked_date as 
	(select 
		campaign_id
	,	northstar_id
	,	max(event_date) as clicked_date
	from 
		drip_emails de
	where 
		event_type = 'email_clicked'
	group by 
		1,2
	)

, drip_email_events as
	(select 
		esd.*,
		ecd.clicked_date
	from 
		email_sent_date esd
		left join email_clicked_date ecd
		using (campaign_id, northstar_id)
	)
	
-- find users first post per campaign
, first_post as 
	(select 
		de.campaign_id,
		de.northstar_id,
		min(date(p.created_at)) as post_created_at
	from 
		drip_emails de
		left join posts p
		using (campaign_id, northstar_id)
	group by 
		1, 2
	)
	
-- join all users who received drip and also submitted a post
, user_posts as 
	(select 
		dee.*,
		fp.post_created_at
	from 
		drip_email_events dee
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
			when post_created_at-drip_date = 0 
			then 1 else 0
		end as same_day,
		case 
			when post_created_at-drip_date = 1 
			then 1 else 0
		end as second_day,
		case 
			when post_created_at-drip_date = 2 
			then 1 else 0
		end as third_day,
		case 
			when post_created_at-drip_date > 2 
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
	,	count(clicked_date)::float as drip_click_users
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
	
-- -- 	find total signups and unique post users per campaign
--, find_total_users as 
--	(select 
--		tdp.campaign_id
--	,	count(distinct s.northstar_id)::float as total_signups
--	,	count(distinct p.northstar_id)::float as total_post_users
--	from 
--		total_drip_users tdp
--		left join signups s 
--		using (campaign_id)
--		left join posts p
--		using (campaign_id)
--	group by 
--		1
--	)
	
-- -- add total signups and unique post users per campaign
--, total_users as 
--	(select 
--		tdp.*
--	,	ftu.total_signups
--	,	ftu.total_post_users
--	from 
--		total_drip_users tdp
--		left join find_total_users ftu
--		using (campaign_id)
--	)


-- calculate percentage of drip users who complete a post
select 
	campaign_id
,	campaign_run_start_date
--,	total_signups
--,	total_post_users
,	drip_users
,	drip_post_users
--,	(total_post_users/total_signups)*100 as total_post_percent
,	(drip_click_users/drip_users)*100 as drip_click_percent
,	(drip_post_users/drip_users)*100 as drip_post_percent
,	case
		when drip_post_users > 0 
		then (total_same_day/drip_post_users)*100
		else 0 
	end as same_day_percent
,	case 
		when drip_post_users > 0 
		then (total_second_day/drip_post_users)*100
		else 0 
	end as second_day_percent
,	case 
		when drip_post_users > 0 
		then (total_third_day/drip_post_users)*100
		else 0 
	end as third_day_percent
,	case 
		when drip_post_users > 0 
		then (total_after_third_day/drip_post_users)*100
		else 0 
	end as after_third_day_percent
from 
	total_drip_users
--	total_users tu

	
	