-- mel signup with channel 
 SELECT 
    sd.northstar_id,
    sd.created_at,
    sd.id,
    sd."source",
    sd.deleted_at, 
    (CASE WHEN sd."source" ilike '%sms%' THEN 'sms'
    WHEN sd."source" NOT LIKE '%sms%'AND sd."source" NOT LIKE '%email%' AND sd."source" NOT LIKE '%niche%' OR sd."source" IS NULL THEN 'web'
    WHEN sd."source" ILIKE '%email%' THEN 'web'
    WHEN sd."source" ILIKE '%niche%' THEN 'niche_coregistration' END) AS "channel"
FROM 
    (SELECT 
        stemp.id,
        max(stemp.updated_at) AS updated_at
    FROM rogue.signups stemp
    GROUP BY stemp.id) s_maxupt
INNER JOIN rogue.signups sd
    ON sd.id = s_maxupt.id AND sd.updated_at = s_maxupt.updated_at