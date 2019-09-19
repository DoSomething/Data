select count(distinct session_id) from (
select
	session_id, event_datetime, event_name, nsid, cummulative_time_gap, registered_same_day,
	string_agg(event_name, '->') over (partition by session_id order by event_datetime rows between unbounded preceding and current row) as journey
from (
	select
		session_id, event_datetime, event_name, nsid, registered_same_day,
		sum(time_gap) over (partition by session_id order by event_datetime asc rows between unbounded preceding and current row) as cummulative_time_gap
	from (
		select
			session_id, event_datetime, event_name, nsid,
			(case when date_trunc('day', u.created_at) = date_trunc('day', event_datetime) then 1 else 0 end)::bool as registered_same_day,
			coalesce((extract(epoch from event_datetime) - extract(epoch from (LAG(event_datetime) over (partition by session_id order by event_datetime))))::int, 0) as time_gap
		from (
			SELECT event_id, session_id, event_datetime, event_name,
				first_value(northstar_id) over (partition by session_id order by northstar_id asc) nsid
			FROM public.phoenix_events_combined
			where event_datetime >= '2019-09-01'
			order by session_id, event_datetime
		) journeys
		left join public.users u on u.northstar_id = nsid
		where
			nsid is not null 
		order by
			session_id, event_datetime asc
	) auth_journeys
) timed_journeys
where
	event_name in ('phoenix_clicked_nav_link_join_now', 'northstar_submitted_register')) vis_journeys
	where (select * from REGEXP_MATCHES(journey, '([\w\d_\->]*)phoenix_clicked_nav_link_join_now([\w\d_\->]*)northstar_submitted_register', 'ig')) is not null and
	registered_same_day is true
