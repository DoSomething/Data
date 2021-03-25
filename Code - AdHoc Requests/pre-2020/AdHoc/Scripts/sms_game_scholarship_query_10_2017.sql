SELECT 
	mmll.mc_run_id, 
	mmll.ms_opt_in_id, 
	mmll.n_title, 
	mmll.ms_phone_number, 
	mmll.mu_uid
FROM 
	users_and_activities.mobile_master_lookup_lite mmll 
WHERE 
	mmll.ms_opt_in_id = 242903 
ORDER BY 
	rand()
LIMIT 30