-- CIO List Pull
---- CTE steps for Full List
-- 		Pull all members w northstar_id, account creation date, email address(DONE)
-- 		Update with the latest engagement over SMS & Qualified Email engagement (DONE)
-- 		Update with SMS & Email subscription status (DONE)
-- 		Update SMS Groups (DONE)
-- 		Update with the latest engagement over email (DONE)
-- 		Add logic that identifies greatest last_engagement_date, email or SMS (DONE)
---- Validation Checks
--		Verify that this is the same # of users that exists in public.user_unsubscribed_at  (DONE)
-- 		Validate Logic for dates
---- CTE Steps for CIO Expiration List Pull
-- 		Identify users with Last Engagement Date over 2 years ago	(DONE)
--		Identify Test or DoSomething users  (.*)+(.*)@dosomething.org  (IGNORING FOR NOW)
---- Notes for Later
-- 		We will want to add a flag in here that identifies members who have already been removed from CIO after the first run of this

----------------------------------------------------------------------------------------------
---- CTE steps for Full List
----------------------------------------------------------------------------------------------

	create table analyst_sandbox.pkg_all_users_messaging_status_2021_02_15 as

	-- All Members
	with members as
		(select northstar_id
		, created_at
		, email
			from public.users)

	-- Add in Most recent engagement
	, members_last_mel_action as  --pull all members and their latest sms and email engagement actions
		(select northstar_id
			, max(case when channel = 'sms' then timestamp end) as most_recent_sms_eng_action
			, max(case when channel = 'email' then timestamp end) as most_recent_email_action
			from public.member_event_log mel group by 1)

	, members_status as -- all members with messaging status and last sms and email action
		(select northstar_id
			, case when email_status is null then 'never-email'
					else email_status end as email_status
			, case when sms_status in ('active', 'pending', 'less') then 'sms-sub'
				   when sms_status in ('stop', 'undeliverable', 'unknown') then 'sms-unsub'
					else 'never sms' end as sms_status
			, most_recent_email_open
			from public.user_activity)

	, member_summary as
		(select members.northstar_id
			, members.created_at
			, members.email
			, members_status.email_status as email_subscriber_status
			, members_status.sms_status as sms_subscriber_status
			, members_last_mel_action.most_recent_sms_eng_action
			, members_last_mel_action.most_recent_email_action
			, members_status.most_recent_email_open
		from members left join members_last_mel_action using(northstar_id)
					 left join members_status using (northstar_id))


	, member_summary_ext as
		(select *
			, concat(email_subscriber_status, '-', sms_subscriber_status)
			, date_part('day', now() - coalesce(most_recent_sms_eng_action, created_at)) as days_since_last_sms_action
			, date_part('day', now() - coalesce(most_recent_email_action, created_at)) as days_since_last_email_open
			, greatest(most_recent_sms_eng_action, created_at, most_recent_email_open) as last_engagement_date
			from member_summary)

	select * from member_summary_ext

----------------------------------------------------------------------------------------------
---- Validation Checks
----------------------------------------------------------------------------------------------

	-- validate number of records

	select count(*) from analyst_sandbox.pkg_all_users_messaging_status_2021_02_15
	Updated Rows	11,474,501

	select count(*) from public.users
	11,474,501

	-- validate the dates are computed properly

	select created_at
	, most_recent_sms_eng_action
	, most_recent_email_action
	, most_recent_email_open
	, last_engagement_date
	from analyst_sandbox.pkg_all_users_messaging_status_2021_02_15
	where created_at >= '2018-01-01'
	limit 20

	-- ~how many users would end up in cio expiration

	select count(*)
	from analyst_sandbox.pkg_all_users_messaging_status_2021_02_15
	where (date_part('day', now() - last_engagement_date) > 730)

	9,431,131
----------------------------------------------------------------------------------------------
---- CIO Expriation List Pull
----------------------------------------------------------------------------------------------

	create table analyst_sandbox.pkg_expire_members_from_cio_2021_02_15	as

	select
	northstar_id
	, email
	, email_subscriber_status
	, sms_subscriber_status
	, last_engagement_date
	from analyst_sandbox.pkg_all_users_messaging_status_2021_02_15
	where (date_part('day', now() - last_engagement_date) > 730)

	Updated rows
	9,431,134


----------------------------------------------------------------------------------------------
---- Parking Lot
----------------------------------------------------------------------------------------------
	REGEXP_MATCH(email, '(.*)\+(.*)\@dosomething\.org', 'g')
