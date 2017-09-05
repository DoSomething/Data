select 
	count(distinct l.ms_phone_number) as 'total number of signups',
	count(distinct case when niche_flags.number_is_niche = 1 then l.ms_phone_number else null end) as 'total number of niche signups', 
	count(distinct l.ms_phone_number) - count(distinct case when niche_flags.number_is_niche = 1 then l.ms_phone_number else null end) as 'total non-niche signups',
	l.`mc_run_id`, 
	l.n_title
from mobile_master_lookup_lite l
left join 
	(select 
		m.ms_phone_number,
		m.mc_run_id,
		max(case when m.ms_opt_in_id IN (210839, 206777, 215113,216832, 207601, 213831) then 1 else 0 end) as number_is_niche
	from mobile_master_lookup_lite m
	group by m.ms_phone_number, m.mc_run_id) niche_flags 
	ON niche_flags.ms_phone_number = l.ms_phone_number and niche_flags.mc_run_id = l.mc_run_id 
	group by l.mc_run_id
	limit 500;;