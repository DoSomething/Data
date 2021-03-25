-- Ad Hoc Request from Diego
-- How many users have received transactional emails after unsubscribing in the last 6 months


with
unsub_members as
	-- all members who unsubscribed from email or did not optin in the last 6 months
	(select northstar_id
		, email_status
		, coalesce(email_unsubscribed_at, created_at) as email_unsub_date
		from user_activity
			where (email_status is null or email_status = 'customer_unsubscribed')
			--and (date_part('day', now() - coalesce(email_unsubscribed_at, created_at))) <= 182
	)


, trans_emails as
	(select customer_id as northstar_id
		, cio_campaign_type
		, cio_message_name
		, timestamp as cio_event_date
		, event_id
		from cio_email_events
			where event_type = 'email_sent'
			and cio_campaign_type in ('transactional', 'transactional_message')
			and (date_part('day', now() - timestamp)) <= 182)

, trans_emails_ext as
	(select t.*
		, u.email_unsub_date
		from trans_emails t
			left join unsub_members u using (northstar_id))

, unsub_trans_summary as
	(select
	northstar_id
	, count(event_id)
	from trans_emails_ext
	where email_unsub_date is not null
	and cio_event_date > email_unsub_date
	group by 1)


select count(*) from unsub_trans_summary

--> 13,325 when we look at just those members who unsub in the last 6 months, who received transactional messaging in last 6 months
--> 19,046 when we look at all members who have unsub, but received transactional messaging in last 6 months
