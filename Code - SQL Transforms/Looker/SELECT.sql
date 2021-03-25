SELECT 
    mel_tagged.source,
    mel_tagged.action_month,
    count(DISTINCT mel_tagged.northstar_id) AS unique_nsids
FROM 
    (SELECT 
        mel.northstar_id,
        mel.action_month,
        max(CASE WHEN mel.source_cat = 'sms' THEN 1 ELSE 0 END) AS sms_action,
        max(CASE WHEN mel.source_cat = 'web' THEN 1 ELSE 0 END) AS web_action,
        CASE WHEN 
            max(CASE WHEN mel.source_cat = 'sms' THEN 1 ELSE 0 END)=1 AND max(CASE WHEN mel.source_cat = 'web' THEN 1 ELSE 0 END)=1 THEN 'both'
            WHEN max(CASE WHEN mel.source_cat = 'web' THEN 1 ELSE 0 END)=1 THEN 'web' ELSE 'sms' END AS source
    FROM 
        (SELECT 
            m.northstar_id,
            m.action_type,
            m.source,
            m.`timestamp`,
            month(m.`timestamp`) AS action_month,
            CASE WHEN
                m.action_type IN ('site_access','site_login') OR 
                m.action_type IN ('reportback','sign-up') AND m.source NOT LIKE '%sms%' OR
                m.action_type IN ('account_creation') AND m.source NOT LIKE '%sms%' THEN 'web' 
                ELSE 'sms' END AS source_cat
        FROM member_event_log m
        WHERE m.`timestamp` >= '2018-01-01' AND m.`timestamp` < '2018-04-01') mel
    GROUP BY mel.action_month, mel.northstar_id) mel_tagged
GROUP BY mel_tagged.source, mel_tagged.action_month