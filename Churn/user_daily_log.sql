--Drop table if exists
DROP TABLE IF EXISTS analyst_sandbox.pkg_user_daily_log

-- Create table, includes all segments
CREATE TABLE analyst_sandbox.pkg_user_daily_log as
(
WITH
d AS -- DATES
	(SELECT generate_series( date_trunc('day', date('2019-06-01')), now(), '1 day' )::date AS cal_date)

, u AS -- USERS
	(WITH campaign as  -- CAMPAIGN METADATA
		  (SELECT cm.contentful_id
		    ,cm.legacy_campaign_id
		    ,COALESCE(ci.campaign_node_id_title, ci.campaign_name) AS campaign_name
		    FROM contentful_metadata cm
			LEFT JOIN campaign_info ci
		  ON ci.campaign_id = cm.legacy_campaign_id)

	SELECT users.northstar_id
	,  users.created_at
	, TO_CHAR(DATE_TRUNC('month', users.created_at), 'YYYY-MM') as account_month

	-- External Traffic Medium
	,CASE
		 when lower(source_detail) like '%rock-the-vote' then 'rock-the-vote'
	     when lower(source_detail) like '%opt_in%' then 'email'
		 when lower(utm_medium) is null then 'organic'
		 when lower(utm_medium) like 'ref%l' then 'referral'
		 when lower(utm_medium) like 'scholarship%' then 'referral'
		 when lower(utm_medium) like 'email' then 'email'
		 when lower(utm_medium) like 'sms' then 'sms'
		 when lower(utm_medium) like 'cpc' then 'cpc'
		 else 'other' end as external_medium

	-- External Traffic Source
	,CASE when (lower(utm_medium) like 'scholarship_list%') then 'scholarship_listing'
	     when (lower(utm_source)  like 'scholarship_list%') then 'scholarship_listing'
		 when (lower(utm_source)  like 'scholarship_feat%') then 'scholarship_featured'
		 when lower(source_detail) like '%rock-the-vote' then 'rock-the-vote'
	     when lower(source_detail) like '%opt_in%' then 'email_opt_in_page'
	     when (lower(source_detail) like '%referrer^_user^_id%' ESCAPE '^') then 'referred-friend'
		 else 'other or none' end as external_source

	-- Do Something Campaign Attribution
	,CASE when lower(campaign.campaign_name) is not null then lower(campaign.campaign_name)
		 when lower(source_detail) like '%rock-the-vote' then 'rock-the-vote'
		 when lower(source_detail) like '%opt_in%' then 'email_signup'
		 else 'other or none' end as dosomething_campaign

	-- External Traffic Campaign
	, CASE when lower(utm_campaign) is not null then lower(utm_campaign)
		else 'none' end as external_campaign

	from users
	  LEFT JOIN campaign ON
	    substring(users.source_detail from '(?<=contentful_id\:)(\w*)') = campaign.contentful_id)

, mde_log AS -- Member Daily Event Log (includes only the mel events)
	(SELECT date(timestamp) as mel_date
		, northstar_id
		-- all
		, SUM(CASE WHEN (mel.action_type = 'account_creation') THEN 1 ELSE 0 END) AS account_creation
		, SUM(CASE WHEN (mel.action_type = 'signup') THEN 1 ELSE 0 END) AS total_campaign_signups  -- need to make sure there isnt some nuance in signups
		, SUM(CASE WHEN (mel.action_type = 'post') THEN 1 ELSE 0 END) AS total_posts   -- need to make sure there isnt some nuance in posts
		--web
		, SUM(CASE WHEN (mel.channel = 'web' AND mel.action_type = 'account_creation') THEN 1 ELSE 0 END) AS web_account_creation
		, SUM(CASE WHEN (mel.channel = 'web' AND mel.action_type = 'site_access') THEN 1 ELSE 0 END) AS web_visits
		, SUM(CASE WHEN (mel.channel = 'web' AND mel.action_type = 'site_login') THEN 1 ELSE 0 END) AS web_logins
		, SUM(CASE WHEN (mel.channel = 'web' AND mel.action_type = 'signup') THEN 1 ELSE 0 END) AS web_campaign_signups  -- need to make sure there isnt some nuance in signups
		, SUM(CASE WHEN (mel.channel = 'web' AND mel.action_type = 'post') THEN 1 ELSE 0 END) AS web_posts   -- need to make sure there isnt some nuance in posts
		--sms
		, SUM(CASE WHEN (mel.channel = 'sms' AND mel.action_type = 'account_creation') THEN 1 ELSE 0 END) AS sms_account_creation
		, SUM(CASE WHEN (mel.channel = 'sms' AND mel.action_type = 'messaged_gambit') THEN 1 ELSE 0 END) AS sms_outbound_messages
		, SUM(CASE WHEN (mel.channel = 'sms' AND mel.action_type = 'bertly_link_click') THEN 1 ELSE 0 END) AS sms_link_clicks
		, SUM(CASE WHEN (mel.channel = 'sms' AND mel.action_type = 'signup') THEN 1 ELSE 0 END) AS sms_campaign_signups  -- need to make sure there isnt some nuance in signups
		, SUM(CASE WHEN (mel.channel = 'sms' AND mel.action_type = 'post') THEN 1 ELSE 0 END) AS sms_posts   -- need to make sure there isnt some nuance in posts
		-- other
		, SUM(CASE WHEN (mel.channel = 'other' AND mel.action_type = 'account_creation') THEN 1 ELSE 0 END) AS other_account_creation
		, SUM(CASE WHEN (mel.channel = 'other' AND mel.action_type = 'post') THEN 1 ELSE 0 END) AS other_posts
		FROM member_event_log mel
				WHERE date(timestamp) >= '2019-06-01'
				GROUP BY 1,2
		)



, mes_log AS -- Member Email Status Log (subscribe & unsubscribes)
	(SELECT date(timestamp) as cioce_date
		, customer_id as northstar_id
		-- all
		, SUM(CASE WHEN (ce.event_type = 'customer_subscribed') THEN 1 ELSE 0 END) AS member_email_subscribed
		, SUM(CASE WHEN (ce.event_type = 'customer_unsubscribed') THEN 1 ELSE 0 END) AS member_email_unsubscribed
		FROM cio.customer_event ce
		GROUP BY 1,2)

, mea_log as -- Member Email Activity Log (opens, clicks, bounces)
	(SELECT date(timestamp) as cioee_date
		, customer_id as northstar_id
		-- all
		, SUM(CASE WHEN (ee.event_type = 'email_opened') THEN 1 ELSE 0 END) AS email_opens
		, SUM(CASE WHEN (ee.event_type = 'email_clicked') THEN 1 ELSE 0 END) AS email_clicks
		, SUM(CASE WHEN (ee.event_type = 'email_bounced') THEN 1 ELSE 0 END) AS email_bounces
		FROM cio.email_event ee
		GROUP BY 1,2)

, mst_sub_log as -- Member Subscription Topics Subscription Log (individual email lists)
	(SELECT date(topic_subscribed_at) as newsletter_sub_date
		, northstar_id
		-- all
		, SUM(CASE WHEN (uns.newsletter_topic = '"community"') THEN 1 ELSE 0 END) AS wyd_sub
		, SUM(CASE WHEN (uns.newsletter_topic = '"lifestyle"') THEN 1 ELSE 0 END) AS boost_sub
		, SUM(CASE WHEN (uns.newsletter_topic = '"news"') THEN 1 ELSE 0 END) AS breakdown_sub
		, SUM(CASE WHEN (uns.newsletter_topic = '"scholarship"') THEN 1 ELSE 0 END) AS ptdg_sub
		FROM public.user_newsletter_subscriptions uns
		GROUP BY 1,2)


, mst_unsub_log as -- Member Subscription Topics Unsubscribe Log (individual email lists)
	(SELECT date(topic_unsubscribed_at) as newsletter_unsub_date
		, northstar_id
		-- all
		, SUM(CASE WHEN (uns.newsletter_topic = '"community"') THEN 1 ELSE 0 END) AS wyd_unsub
		, SUM(CASE WHEN (uns.newsletter_topic = '"lifestyle"') THEN 1 ELSE 0 END) AS boost_unsub
		, SUM(CASE WHEN (uns.newsletter_topic = '"news"') THEN 1 ELSE 0 END) AS breakdown_unsub
		, SUM(CASE WHEN (uns.newsletter_topic = '"scholarship"') THEN 1 ELSE 0 END) AS ptdg_unsub
		FROM public.user_newsletter_subscriptions uns
		GROUP BY 1,2)


SELECT usl.*
-- member daily event log data
, (CASE WHEN mde_log.account_creation is null THEN 0 ELSE mde_log.account_creation END)
, (CASE WHEN mde_log.total_campaign_signups is null THEN 0 ELSE mde_log.total_campaign_signups END)
, (CASE when mde_log.total_posts is null THEN 0 ELSE mde_log.total_posts END)
, (CASE when mde_log.web_account_creation is null THEN 0 ELSE mde_log.web_account_creation END)
, (CASE when mde_log.web_visits is null THEN 0 ELSE mde_log.web_visits END)
, (CASE when mde_log.web_logins is null THEN 0 ELSE mde_log.web_logins END)
, (CASE when mde_log.web_campaign_signups is null THEN 0 ELSE mde_log.web_campaign_signups END)
, (CASE when mde_log.web_posts is null THEN 0 ELSE mde_log.web_posts END)
, (CASE when mde_log.sms_account_creation is null THEN 0 ELSE mde_log.sms_account_creation END)
, (CASE when mde_log.sms_outbound_messages is null THEN 0 ELSE mde_log.sms_outbound_messages END)
, (CASE when mde_log.sms_link_clicks is null THEN 0 ELSE mde_log.sms_link_clicks END)
, (CASE when mde_log.sms_campaign_signups is null THEN 0 ELSE mde_log.sms_campaign_signups END)
, (CASE when mde_log.sms_posts is null THEN 0 ELSE mde_log.sms_posts END)
, (CASE when mde_log.other_account_creation is null THEN 0 ELSE mde_log.other_account_creation END)
, (CASE when mde_log.other_posts is null THEN 0 ELSE mde_log.other_posts END)

-- member email status data
, (CASE when mes_log.member_email_subscribed is null THEN 0 ELSE mes_log.member_email_subscribed END)
, (CASE when mes_log.member_email_unsubscribed is null THEN 0 ELSE mes_log.member_email_unsubscribed END)

-- member email activity data
, (CASE when mea_log.email_opens is null THEN 0 ELSE mea_log.email_opens END)
, (CASE when mea_log.email_clicks is null THEN 0 ELSE mea_log.email_clicks  END)
, (CASE when mea_log.email_bounces is null THEN 0 ELSE mea_log.email_bounces  END)

-- member subscription topics sub
, (CASE when mst_sub_log.wyd_sub is null THEN 0 ELSE mst_sub_log.wyd_sub END)
, (CASE when mst_sub_log.boost_sub is null THEN 0 ELSE mst_sub_log.boost_sub END)
, (CASE when mst_sub_log.breakdown_sub is null THEN 0 ELSE mst_sub_log.breakdown_sub END)
, (CASE when mst_sub_log.ptdg_sub is null THEN 0 ELSE mst_sub_log.ptdg_sub END)

-- member subscription topics unsub
, (CASE when mst_unsub_log.wyd_unsub is null THEN 0 ELSE mst_unsub_log.wyd_unsub END)
, (CASE when mst_unsub_log.boost_unsub is null THEN 0 ELSE mst_unsub_log.boost_unsub END)
, (CASE when mst_unsub_log.breakdown_unsub is null THEN 0 ELSE mst_unsub_log.breakdown_unsub END)
, (CASE when mst_unsub_log.ptdg_unsub is null THEN 0 ELSE mst_unsub_log.ptdg_unsub END)

-- THIS IS WHERE YOU LEFT OFF ---
-- derived actions
--, (case when (
--		account_creation > 1 or
--		total_campaign_signups > 1 or
--
--
--)


FROM
	(SELECT d.cal_date, u.northstar_id, u.created_at, u.external_medium, u.external_source, u.external_campaign, u.dosomething_campaign
	FROM d CROSS JOIN u
	WHERE date(d.cal_date) >= date(u.created_at)) as usl

	LEFT JOIN mde_log
		ON usl.cal_date = date(mde_log.mel_date)
		AND usl.northstar_id = mde_log.northstar_id

	LEFT JOIN mes_log
		ON usl.cal_date = date(mes_log.cioce_date)
		AND usl.northstar_id = mes_log.northstar_id

	LEFT JOIN mea_log
		ON usl.cal_date = date(mea_log.cioee_date)
		AND usl.northstar_id = mea_log.northstar_id

	LEFT JOIN mst_sub_log
		ON usl.cal_date = date(mst_sub_log.newsletter_sub_date)
		AND usl.northstar_id = mst_sub_log.northstar_id

	LEFT JOIN mst_unsub_log
		ON usl.cal_date = date(mst_unsub_log.newsletter_unsub_date)
		AND usl.northstar_id = mst_unsub_log.northstar_id

WHERE usl.created_at >= '2019-06-01'
)
