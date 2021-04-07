-- We will get this in the data model but here is the context on why pulling this ad hoc
-- https://dosomething.slack.com/archives/C03T8SDJG/p1615834053002900
-- mask is just a particular campaign that this is for but this query can be used to derive
-- average data to rb for any campaign by swapping out the campaign id


with mask_signups as
	(select northstar_id, id as signup_id, created_at as signup_date
	from public.signups where campaign_id = '9097')

, mask_reportbacks as
	(select post_action, post_created_at, northstar_id, signup_id
		from reportbacks where campaign_id = '9097')

, signup_rb_summary as
	(select r.*
	, s.signup_id
	, s.signup_date
	, date_part('day', r.post_created_at - s.signup_date) as days_to_rb
		from mask_reportbacks r left join mask_signups s on s.signup_id = r.signup_id)

, campaign_summaries as
(select count(*) as total_reportbacks
	, sum(days_to_rb) as total_days
	, avg(days_to_rb)::numeric(10,2) as avg_days_to_rb
	from signup_rb_summary)

select * from campaign_summaries
