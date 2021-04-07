# Trello request https://trello.com/c/r9f7FK3E/1650-proportion-of-nmas-from-signups-for-new-state-of-mind-are-not-as-high-as-we-expected-why
-- The set of queries here were to investigate the questions in the linked trello


----FASTWEB -------

--> # of distinct visits from fastweb

select TO_CHAR(DATE_TRUNC('month', landing_datetime), 'YYYY-MM') as account_month, count(distinct device_id) from phoenix_sessions_combined
where session_utm_campaign like '%fastweb%'
and landing_datetime >= '2019-06-01'
group by 1

--> # of total sessions fastweb
select TO_CHAR(DATE_TRUNC('month', landing_datetime), 'YYYY-MM') as account_month, count(*) from phoenix_sessions_combined
where session_utm_campaign like '%fastweb%'
group by 1


--> campaign signups

WITH signups AS (select s.northstar_id,
                s.id,
                s.campaign_id,
                s.campaign_run_id,
                s.why_participated,
                s.source,
                s.source_bucket,
                s.source_details,
                s.created_at,
                s.utm_source,
                s.utm_medium,
                s.utm_campaign,
                p1.ignore,
                s.details
          from public.signups s
          left join (
              select distinct northstar_id, campaign_id, 1 as ignore
              from public.posts
              where signup_id = -1
          ) p1
          on s.northstar_id = p1.northstar_id
          and s.campaign_id = p1.campaign_id
          )
SELECT
	TO_CHAR(DATE_TRUNC('month', signups.created_at ), 'YYYY-MM') AS "signups.signup_created_at_month",
	COUNT(DISTINCT (CASE WHEN signups.source_bucket = 'web' AND signups.ignore is null
        THEN signups.id
        ELSE NULL END)) AS "signups.count_web_signups"
FROM signups
WHERE (signups.utm_campaign LIKE '%fastweb%') AND ((((signups.created_at ) >= (TIMESTAMP '2019-06-01') AND (signups.created_at ) < (TIMESTAMP '2020-04-30'))))
GROUP BY DATE_TRUNC('month', signups.created_at )
ORDER BY 1


--> # of users created from fastweb

SELECT
	TO_CHAR(DATE_TRUNC('month', users.created_at ), 'YYYY-MM') AS "users.created_at_month",
	COUNT(DISTINCT users.northstar_id ) AS "users.count_distinct_northstar_id"
FROM public.users
WHERE (users.utm_campaign LIKE '%fastweb%')
GROUP BY 1
ORDER BY 1 asc

----NEW STATE OF MIND ----

--> # of distinct visits from fastweb new state of mind

select TO_CHAR(DATE_TRUNC('month', landing_datetime), 'YYYY-MM') as account_month, count(distinct device_id) from phoenix_sessions_combined
where session_utm_campaign like '%fastweb%'
and landing_datetime >= '2019-06-01'
and landing_page like '%new-state-of-mind'
group by 1

--> # of total sessions fastweb new state of mind
select TO_CHAR(DATE_TRUNC('month', landing_datetime), 'YYYY-MM') as account_month, count(*) from phoenix_sessions_combined
where session_utm_campaign like '%fastweb%'
and landing_page like '%new-state-of-mind'
group by 1


--> campaign signups
WITH signups AS (select s.northstar_id,
                s.id,
                s.campaign_id,
                s.campaign_run_id,
                s.why_participated,
                s.source,
                s.source_bucket,
                s.source_details,
                s.created_at,
                s.utm_source,
                s.utm_medium,
                s.utm_campaign,
                p1.ignore,
                s.details
          from public.signups s
          left join (
              select distinct northstar_id, campaign_id, 1 as ignore
              from public.posts
              where signup_id = -1
          ) p1
          on s.northstar_id = p1.northstar_id
          and s.campaign_id = p1.campaign_id
          )
SELECT
	TO_CHAR(DATE_TRUNC('month', signups.created_at ), 'YYYY-MM') AS "signups.signup_created_at_month",
	COUNT(DISTINCT (CASE WHEN signups.source_bucket = 'web' AND signups.ignore is null
        THEN signups.id
        ELSE NULL END)) AS "signups.count_web_signups"
FROM signups

WHERE (signups.utm_campaign LIKE '%fastweb%') AND ((((signups.created_at ) >= (TIMESTAMP '2019-11-01') AND (signups.created_at ) < (TIMESTAMP '2020-04-21')))) and signups.campaign_id = '9061'
GROUP BY DATE_TRUNC('month', signups.created_at )
ORDER BY 1

--> # of campaign signups per day from fastweb driven traffic
WITH signups AS (select s.northstar_id, s.id, s.campaign_id, s.campaign_run_id, s.why_participated, s.source, s.source_bucket,
                s.source_details, s.created_at, s.utm_source, s.utm_medium, s.utm_campaign, p1.ignore, s.details
          from public.signups s
          left join (select distinct northstar_id, campaign_id, 1 as ignore from public.posts where signup_id = -1) p1
          on s.northstar_id = p1.northstar_id and s.campaign_id = p1.campaign_id)

select TO_CHAR(DATE_TRUNC('month', signup_created_at_day), 'YYYY-MM') AS "signup_created_at_month"
,count(daily_user_signups.users) as total_users
,sum(daily_user_signups.count_web_signups) as total_signups
from
	(SELECT
		DATE_TRUNC('day', signups.created_at) AS "signup_created_at_day"
		,signups.northstar_id as users
		,COUNT(DISTINCT (CASE WHEN signups.source_bucket = 'web' AND signups.ignore is null THEN signups.id ELSE NULL END)) AS "count_web_signups"
	FROM signups
	WHERE (signups.utm_campaign LIKE '%fastweb%') AND ((((signups.created_at ) >= (TIMESTAMP '2019-06-01')
	AND (signups.created_at ) < (TIMESTAMP '2020-04-21'))))
	GROUP BY 1,2
	ORDER BY 1) as daily_user_signups
	group by 1


--> # of users created from fastweb

select nma.*, concat(cd.title,' ', cd.internal_title) as contentful_details
from
(select
TO_CHAR(DATE_TRUNC('month', created_at), 'YYYY-MM') as account_month
,substring(source_detail from '(?<=contentful_id\:)(\w*)') as contentful_id -- ideally this gets replaced with the actual ds_campaign_name in the future
,COUNT(DISTINCT northstar_id) as new_members_acquired
FROM public.users
WHERE created_at >= '2020-01-01'
and (users.utm_campaign LIKE '%fastweb%')
GROUP BY 1,2) nma
left join
(select * from public.contentful_metadata where internal_title like 'New State of Mind%')cd on
nma.contentful_id = cd.contentful_id
where cd.internal_title is not null
