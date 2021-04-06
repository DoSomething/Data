#Trello Request https://trello.com/c/BlWpOccA/1656-quick-raf-assessment

-- This was just exploratory to get a sense of the data.
select action_type, channel, type from public.member_event_log mel where timestamp >= '2019-09-01'
group by 1,2,3 limit 100


_________________

-- The request was to look at retention outcomes. It didn't make sense to tell the story of varios outcomes
--without comparing against other groups so that is why I added on classification of other members


select case when (users.source like '%phoenix%' and users.source_detail LIKE '%referrer^_user^_id%' ESCAPE '^') then 'friend-referral'
when (users.utm_medium LIKE '%scholarship%' or users.utm_source LIKE '%scholarship%') then 'scholarship'
when users.source_detail like '%rock-the-vote' then 'voter-reg'
when users.source_detail like '%google%' then 'google'
when users.source_detail like '%facebook%' then 'facebook'
when users.source like '%sms%' then 'sms'
when users.source_detail like '%opt_in%' then 'email_newsletter'
else 'all else' end as group

, COUNT(DISTINCT users.northstar_id ) AS distinct_members
, COUNT(DISTINCT CASE WHEN ((member_event_log.days_since_registration > 0 AND member_event_log.days_since_registration < 90))
THEN users.northstar_id  ELSE NULL END) AS mel_within_90
, COUNT(DISTINCT CASE WHEN (member_event_log.days_since_registration >= 90)
THEN users.northstar_id  ELSE NULL END) AS mel_after_90

, COUNT(DISTINCT CASE WHEN ((date(signups.created_at) - date(users.created_at)) < 90)
THEN users.northstar_id  ELSE NULL END) AS signup_within_90

, COUNT(DISTINCT CASE WHEN ((date(signups.created_at) - date(users.created_at)) >= 90)
THEN users.northstar_id  ELSE NULL END) AS signup_after_90


, COUNT(DISTINCT CASE WHEN ((date(posts.created_at) - date(users.created_at)) < 90 and is_reportback = 'true')
THEN users.northstar_id  ELSE NULL END) AS rb_within_90

, COUNT(DISTINCT CASE WHEN ((date(posts.created_at) - date(users.created_at)) >= 90 and is_reportback = 'true')
THEN users.northstar_id  ELSE NULL END) AS rb_after_90

FROM public.users AS users
LEFT JOIN looker_scratch.lr$q6ji61586256985999_member_event_log AS member_event_log
ON member_event_log.northstar_id = users.northstar_id
LEFT JOIN public.signups as signups
ON signups.northstar_id = users.northstar_id
LEFT JOIN public.posts as posts
on posts.northstar_id = users.northstar_id
WHERE ((((users.created_at ) >= (TIMESTAMP '2019-12-04') AND (users.created_at ) < (TIMESTAMP '2020-01-04'))))
group by 1
order by 2 desc


----------- just figuring out the various campaigns people come in on for grouping.
-----------  This was just exploratory before setting group segments.

select
utm_campaign, utm_medium, utm_source, count(*)
from users where created_at >= '2019-09-24'  and created_at >= '2019-10-24'
group by 1, 2, 3

select source, source_detail, count(*)
from users where created_at >= '2019-09-24'  and created_at >= '2019-10-24'
and utm_campaign is null
group by 1,2
order by 3 desc
