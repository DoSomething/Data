    SELECT -- CAMPAIGN SIGNUP WITH CHANNEL
        DISTINCT s.northstar_id AS northstar_id,
        s.created_at AS "timestamp",
        'signup' AS "action",
        '1' AS action_id, 
        s."source" AS "source",
        s.id::varchar AS action_serial_id,
        s.channel AS channel
    FROM      
         (SELECT 
            sd.northstar_id,
            sd.created_at,
            sd.id,
            sd."source",
            sd.deleted_at, 
            (CASE WHEN sd."source" ILIKE '%sms%' THEN 'sms'
            WHEN sd."source" NOT LIKE '%sms%'AND sd."source" NOT LIKE '%email%' AND sd."source" NOT LIKE '%niche%' OR sd."source" IS NULL THEN 'web'
            WHEN sd."source" ILIKE '%email%' THEN 'email'
            WHEN sd."source" ILIKE '%niche%' THEN 'niche_coregistration' END) AS "channel"
        FROM 
            (SELECT 
                stemp.id,
                max(stemp.updated_at) AS updated_at
            FROM rogue.signups stemp
            GROUP BY stemp.id) s_maxupt
        INNER JOIN rogue.signups sd
            ON sd.id = s_maxupt.id AND sd.updated_at = s_maxupt.updated_at
        ) s 
    WHERE s.deleted_at IS NULL
UNION ALL
    SELECT -- CAMPAIGN POSTS WITH CHANNEL
        DISTINCT p.northstar_id AS northstar_id,
        p.created_at AS "timestamp",
        'post' AS "action",
        '2' AS action_id,
        p."source" AS "source",
        p.id::varchar AS action_serial_id,
        p.channel AS channel
    FROM 
        (
        SELECT 
            pd.northstar_id,
            pd.created_at,
            pd.id,
            pd."source",
            pd.deleted_at,
            pd."type",
            (CASE WHEN pd."source" ILIKE '%sms%' THEN 'sms'
            WHEN pd."source" ILIKE '%phoenix%' OR pd."source" IS NULL THEN 'web'
            WHEN pd."source" ILIKe '%app%' THEN 'mobile_app' END) AS "channel"
        FROM 
            (SELECT 
                ptemp.id,
                max(ptemp.updated_at) AS updated_at
            FROM rogue.posts ptemp
            GROUP BY ptemp.id) p_maxupt
        INNER JOIN rogue.posts pd
        ON pd.id = p_maxupt.id AND pd.updated_at = p_maxupt.updated_at
            ) p
    WHERE p.deleted_at IS NULL
    AND p."type" IS DISTINCT FROM 'voter-reg'
UNION ALL -- SITE ACCESS
    SELECT DISTINCT 
        u_access.id AS northstar_id,
        u_access.last_accessed_at AS "timestamp",
        'site_access' AS "action",
        '3' AS action_id,
        NULL AS "source",
        '0' AS action_serial_id,
        'web' AS channel
    FROM northstar.users u_access
    WHERE u_access.last_accessed_at IS NOT NULL
UNION ALL -- SITE LOGIN
    SELECT DISTINCT 
        u_login.id AS northstar_id,
        u_login.last_authenticated_at AS "timestamp",
        'site_login' AS "action",
        '4' AS action_id,
        NULL AS "source",
        '0' AS action_serial_id,
        'web' AS channel
    FROM northstar.users u_login
    WHERE u_login.last_authenticated_at IS NOT NULL 
UNION ALL 
    SELECT -- ACCOUNT CREATION 
        DISTINCT u.id AS northstar_id,
        u.created_at AS "timestamp",
        'account_creation' AS action, 
        '5' AS action_id,
        u."source" AS "source",
        '0' AS action_serial_id, 
        (CASE WHERE u."source" ILIKE '%sms%' THEN 'sms'
        WHEN u."source" NOT LIKE '%sms%' AND u."source" NOT LIKE '%niche%' THEN 'web'
        WHEN u."source" ILIKE '%niche%' THEN 'niche_coregistration' END) AS "channel"
    FROM
        (SELECT 
                u_create.id,
                max(u_create."source") AS "source",
                min(u_create.created_at) AS created_at
        FROM northstar.users u_create
        GROUP BY u_create.id) u