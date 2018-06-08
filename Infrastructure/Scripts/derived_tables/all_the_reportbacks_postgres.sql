SELECT
	a.date AS "date"
	, a.campaign_run_id AS "campaign_run_id"
	, sum(a.photo_rbs) AS "photo_rbs"
	, sum(a.text_rbs) AS "text_rbs"
	, sum(a.rbs) AS "rbs"
	, sum(a.calls) AS "calls"
	, sum(a.social) AS "social"
	, sum(a.voter_registrations) AS "voter_registrations"
	, sum(a.self_reported_registrations) AS "self_reported_registrations"
	, sum(a.other) AS "other"
FROM (
	SELECT 
		date_trunc('month',ca.post_attribution_date) AS "date",
		ca.campaign_run_id,
		sum(CASE WHEN ca.post_class ILIKE '%photo%' OR 
			ca.post_class IS NULL
			THEN ca.reportback_volume ELSE 0 END) AS photo_rbs,
		sum(CASE WHEN ca.post_class ILIKE '%text%' 
			THEN ca.reportback_volume ELSE 0 END) AS text_rbs,
		sum(CASE WHEN ca.post_class ILIKE '%voter%' AND 
			ca.campaign_id = '822' AND 
			ca.post_status = 'accepted'
			THEN ca.reportback_volume ELSE 0 END) AS voter_registrations,
		0 AS "rbs",
		0 AS "calls",
		0 AS "social",
		0 AS "self_reported_registrations",
		0 AS "other"
	FROM 
		(SELECT DISTINCT
			dp.northstar_id,
			dp.campaign_id,
			dp.campaign_run_id,
			dp.signup_id,
			dp.post_class,
			dp.post_status,
			dp.reportback_volume, 
			dp.post_attribution_date
		FROM public.campaign_activity dp
		WHERE dp.post_id IS NOT NULL
		AND dp.post_status IS DISTINCT FROM 'rejected'
		) ca
	GROUP BY date_trunc('month',ca.post_attribution_date), ca.campaign_run_id
UNION ALL 
	SELECT
		l.date AS "date",
		l.campaign_run_id AS "campaign_run_id",
		0 AS "photo_rbs",
		0 AS "text_rbs",
		CASE WHEN l.campaign_run_id = '8103' THEN 0 
			ELSE l.voter_registrations END AS "voter_registrations",
		l.rbs AS "rbs",
		l.calls AS "calls",
		l.social AS "social",
		l.self_reported_registrations AS "self_reported_registrations",
		l.other AS "other"
	FROM public.legacy_reportbacks l
	) a
GROUP BY a."date", a.campaign_run_id
ORDER BY a."date" DESC
;
