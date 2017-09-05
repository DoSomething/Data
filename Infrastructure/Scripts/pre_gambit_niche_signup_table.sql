select 
	l.*,
	niche_flags.number_is_niche
from mobile_master_lookup_lite l
left join 
	(select 
		m.ms_phone_number,
		m.mc_run_id,
		max(case when m.ms_opt_in_id IN (210839, 206777) then 1 else 0 end) as number_is_niche
	from mobile_master_lookup_lite m
	group by m.ms_phone_number, m.mc_run_id) niche_flags 
	ON niche_flags.ms_phone_number = l.ms_phone_number and niche_flags.mc_run_id = l.mc_run_id 
	where niche_flags.number_is_niche = 1
	and l.ms_opt_in_id = 206777
	limit 500;;