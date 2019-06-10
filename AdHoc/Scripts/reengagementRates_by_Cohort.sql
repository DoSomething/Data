SELECT
	nsids.cohort,
	avg(nsids.reengage_within_7) AS avg_reengage_within_7,
	avg(nsids.reengage_within_14) AS avg_reengage_within_14,
	avg(nsids.reengage_within_21) AS avg_reengage_within_21,
	avg(nsids.reengage_within_30) AS avg_reengage_within_30,
	avg(nsids.reengage_within_60) AS avg_reengage_within_60,
	avg(nsids.reengage_within_90) AS avg_reengage_within_90,
	avg(nsids.reengage_within_120) AS avg_reengage_within_120
FROM 
	(SELECT
		m.northstar_id,
		cu.created_at,
		cu.cohort::INT,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '7 days') THEN 1 ELSE 0 END) reengage_within_7,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '14 day') THEN 1 ELSE 0 END) reengage_within_14,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '21 day') THEN 1 ELSE 0 END) reengage_within_21,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '30 day') THEN 1 ELSE 0 END) reengage_within_30,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '60 day') THEN 1 ELSE 0 END) reengage_within_60,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '90 day') THEN 1 ELSE 0 END) reengage_within_90,
		MAX(CASE WHEN m."timestamp"::date > cu.created_at AND m."timestamp"::date < (cu.created_at + INTERVAL '120 day') THEN 1 ELSE 0 END) reengage_within_120
	FROM
		member_event_log m
	INNER JOIN (
		SELECT 
			u.northstar_id,
			u.created_at::date,
			EXTRACT('year' FROM u.created_at) AS cohort
		FROM
			users u
		WHERE
			u.created_at >= '2014-01-01') cu ON cu.northstar_id = m.northstar_id
	WHERE
		m."timestamp" >= '2014-01-01'
		AND m.action_type <> 'account_creation'
	GROUP BY
		m.northstar_id,
		cu.created_at,
		cu.cohort) nsids
GROUP BY 
	nsids.cohort