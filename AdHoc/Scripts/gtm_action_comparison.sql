--https://trello.com/c/LgSDsmM7/1288-data-request-grab-the-mic-member-engagement
SELECT 
	gtm.northstar_id,
	gtm.did_gtm,
	gtm.gtm_signup_month,
	gtm.earliest_non_gtm_signup_month,
	act_counts.action_month,
	act_counts.action_type,
	act_counts.action_count
FROM 
	(SELECT 
		c.northstar_id,
		max(CASE 
				WHEN c.campaign_run_id = 8022 
				THEN EXTRACT('month' FROM c.signup_created_at) 
				ELSE 0 END) AS gtm_signup_month,
		min(CASE 
				WHEN c.campaign_run_id <> 8022 
				THEN EXTRACT('month' FROM c.signup_created_at) 
				ELSE 0 END) AS earliest_non_gtm_signup_month,
		max(CASE 
				WHEN c.campaign_run_id = 8022 
				THEN 1 ELSE 0 END) AS did_gtm
	FROM campaign_activity c
	WHERE c.signup_created_at >= '2018-01-01'
	GROUP BY c.northstar_id) gtm
LEFT JOIN 
	(SELECT 
		act.northstar_id,
		EXTRACT('month' FROM act."timestamp") AS action_month,
		act.action_type,
		count(*) AS action_count
	FROM 
		(SELECT
			m.event_id,
			m.northstar_id,
			m.action_type,
			m."timestamp"
		FROM member_event_log m 
		WHERE m."timestamp" >= '2018-01-01'
			UNION ALL 
		SELECT 
			e.event_id,
			e.northstar_id,
			'phoenix_page_action' AS action_type,
			e.event_datetime AS "timestamp"
		FROM phoenix_events e
		WHERE e.event_name IN ('view','visit','open modal')
		AND e.northstar_id IS NOT NULL AND e.northstar_id <> ''
		AND e.event_datetime >= '2018-01-01'
			UNION ALL 
		SELECT 
			em.event_id,
			em.customer_id AS northstar_id,
			'email_interaction' AS action_type,
			em."timestamp"
		FROM public.email_event em
		WHERE em."timestamp" >= '2018-01-01') act
	GROUP BY act.northstar_id, EXTRACT('month' FROM act."timestamp"), act.action_type
	) act_counts 
	ON act_counts.northstar_id = gtm.northstar_id 
;