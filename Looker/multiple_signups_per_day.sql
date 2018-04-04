select 
	count(distinct m.northstar_id), m.first_created
from(
	select distinct a.northstar_id, a.signup_id as first, b.signup_id as second, date(a.signup_created_at) as first_created, date(b.signup_created_at)
	from campaign_activity a
	left join campaign_activity b 
		on a.northstar_id = b.northstar_id AND date(a.signup_created_at) = date(b.signup_created_at)
	where a.signup_id <> b.signup_id
	and year(a.signup_created_at) = '2018'
	and year(b.signup_created_at) = '2018'
	and a.campaign_id <> 3590
	and b.campaign_id <> 3590 -- seeing a ton of people signed up for multiple runs of the shower songs : ( 
	group by a.northstar_id, a.signup_id, b.signup_id, date(a.signup_created_at), date(b.signup_created_at)) as m
group by m.first_created
;;

--with source 

select
	m.northstar_id, m.first_created, m.first_source, m.second_source
from(
	select distinct a.northstar_id, a.signup_id as first, b.signup_id as second, date(a.signup_created_at) as first_created, date(b.signup_created_at), a.signup_source as first_source, b.signup_source as second_source
	from quasar.campaign_activity a
	left join quasar.campaign_activity b
	  on a.northstar_id = b.northstar_id AND date(a.signup_created_at) = date(b.signup_created_at)
	where a.signup_id <> b.signup_id
	and a.campaign_id <> 3590
	and b.campaign_id <> 3590 -- seeing a ton of people signed up for multiple runs of the shower songs : (
	and date(a.signup_created_at) > '2016-12-31'
	and date(b.signup_created_at) > '2016-12-31'
	group by a.northstar_id, a.signup_id, b.signup_id, date(a.signup_created_at), date(b.signup_created_at)) as m
group by m.northstar_id, m.first_created
       ;;