--Signups
SELECT 
	e.id AS id,
	(e.content::json)->>'northstar_id'::varchar AS nsid,
	(e.content::json)->>'campaign_id'::varchar AS campaign_id,
	(e.content::json)->>'campaign_run_id' AS campaign_run_id,
	((e.content::json)->>'quantity')::bigint AS quantity,
	(e.content::json)->>'why_participated' AS why_participated,
	(e.content::json)->>'source' AS source,
	(e.content::json)->>'details' AS details,
	(e.content::json)->>'created_at' AS created_at,
	(e.content::json)->>'updated_at' AS updated_at,
	(e.content::json)->>'deleted_at' AS deleted_at
FROM rogue_prod.events e
WHERE e.eventable_type ILIKE '%signup%';

--Posts
SELECT 
	e.id,
	(e.content::json)->>'signup_id'::varchar AS signup_id,
	(e.content::json)->>'campaign_id'::varchar AS campaign_id,
	(e.content::json)->>'campaign_run_id' AS campaign_run_id,
	(e.content::json)->>'northstar_id'::varchar AS nsid,
	(e.content::json)->>'type'::varchar AS type,
	(e.content::json)->>'action'::varchar AS action,
	((e.content::json)->>'quantity')::bigint AS quantity,
	(e.content::json)->>'url'::varchar AS url,
	(e.content::json)->>'caption'::varchar AS caption,
	(e.content::json)->>'status'::varchar AS status,
	(e.content::json)->>'source'::varchar AS source,
	(e.content::json)->>'signup_source'::varchar AS signup_source,
	CAST((e.content::json)->>'remote_addr' AS inet) AS remote_addr,
	((e.content::json)->>'created_at')::timestamp AS created_at,
	((e.content::json)->>'updated_at')::timestamp AS updated_at,
	((e.content::json)->>'deleted_at')::timestamp AS deleted_at
FROM rogue_prod.events e
WHERE e.eventable_type ILIKE '%post%'
