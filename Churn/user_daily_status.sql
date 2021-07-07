
--Drop table if exists
--DROP TABLE IF EXISTS analyst_sandbox.pkg_user_daily_status


-- Create table
CREATE TABLE analyst_sandbox.pkg_user_daily_status AS

(WITH
user_most_recent_open as -- This CTE enables us to ggenerate diff between the cal date and the most recent open for inactivity flags over time
			 (SELECT udl.northstar_id
			 , udl.cal_date
			 , max(ueo.email_open_date) as most_recent_email_open
			 FROM analyst_sandbox.pkg_user_daily_log udl
			 LEFT JOIN
			 -- subquery to look at all user email opens, possibly will want some logic to determine valide email opens
					(SELECT cal_date as email_open_date, northstar_id, email_opens
						FROM analyst_sandbox.pkg_user_daily_log
					 	WHERE email_opens > 0
						GROUP BY 1,2,3) ueo
						ON udl.northstar_id = ueo.northstar_id
						AND udl.cal_date >= ueo.email_open_date
					GROUP BY 1,2
					ORDER BY 2)

-- Generates the primary endpoint with the necessary email flags
SELECT
	-- all the dates
	udel.cal_date
	, TO_CHAR(DATE_TRUNC('month', udel.cal_date), 'YYYY-MM') AS cal_month
	, udel.northstar_id
	, udel.created_at as member_create_date
	, TO_CHAR(DATE_TRUNC('month', udel.created_at), 'YYYY-MM') AS account_mo_cohort
	,(date(udel.cal_date) - date(udel.created_at)) as member_tenure
	-- all the account attribution
	, udel.external_medium
	, udel.external_source
	, udel.external_campaign
	, udel.dosomething_campaign
	-- all the member status flags
	, udel.member_email_subscribed
	, udel.member_email_unsubscribed
	, umro.most_recent_email_open
	, (SUM(udel.member_email_subscribed) over (partition by udel.northstar_id order by udel.cal_date))
		- (SUM(udel.member_email_unsubscribed) over (partition by udel.northstar_id order by udel.cal_date)) as email_active_flag
	, (SUM(udel.wyd_sub) over (partition by udel.northstar_id order by udel.cal_date))
		- (SUM(udel.wyd_unsub) over (partition by udel.northstar_id order by udel.cal_date)) as wyd_active_flag
	, (SUM(udel.boost_sub) over (partition by udel.northstar_id order by udel.cal_date))
		- (SUM(udel.boost_unsub) over (partition by udel.northstar_id order by udel.cal_date)) as boost_active_flag
	, (SUM(udel.breakdown_sub) over (partition by udel.northstar_id order by udel.cal_date))
		- (SUM(udel.breakdown_unsub) over (partition by udel.northstar_id order by udel.cal_date)) as breakdown_active_flag
	, (SUM(udel.ptdg_sub) over (partition by udel.northstar_id order by udel.cal_date))
		- (SUM(udel.ptdg_unsub) over (partition by udel.northstar_id order by udel.cal_date)) as ptdg_active_flag
	, (date(udel.cal_date) - date(umro.most_recent_email_open)) as days_since_last_email_activity
	, CASE when (date(udel.cal_date) - date(umro.most_recent_email_open)) >= 90 then 1 else 0 end as inactive_90_flag
	, CASE when (date(udel.cal_date) - date(umro.most_recent_email_open)) >= 120 then 1 else 0 end as inactive_120_flag
	-- all the account activity
	, udel.total_campaign_signups -- includes web, sms
	, udel.total_posts 			  -- includes web, sms, other
	, udel.web_visits
	, udel.web_logins
	, udel.sms_outbound_messages
	, udel.sms_link_clicks
	-- all the other email related stuff
	, udel.email_opens
	, udel.email_clicks
	, udel.email_bounces
FROM analyst_sandbox.pkg_user_daily_log udel
	LEFT JOIN user_most_recent_open umro on umro.northstar_id = udel.northstar_id
		AND udel.cal_date = umro.cal_date)
