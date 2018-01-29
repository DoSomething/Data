SELECT 
    u.northstar_id,
    rbs_7890.reportback_30 AS within_campaign_rbs_30,
    rbs_7890.reportback_60 AS within_campaign_rbs_60,
    rbs_7890.reportback_90 AS within_campaign_rbs_90,
    non_7890_rbs.reportback_30 AS outside_campaign_rbs_30,
    non_7890_rbs.reportback_60 AS outside_campaign_rbs_60,
    non_7890_rbs.reportback_90 AS outside_campaign_rbs_90,
    IFNULL(rbs_7890.reportback_30, 0) + IFNULL(non_7890_rbs.reportback_30, 0) AS total_reportbacks_30,
    IFNULL(rbs_7890.reportback_60, 0) + IFNULL(non_7890_rbs.reportback_60, 0) AS total_reportbacks_60,
    IFNULL(rbs_7890.reportback_90, 0) + IFNULL(non_7890_rbs.reportback_90, 0) AS total_reportbacks_90
FROM quasar.users u 
LEFT JOIN 
    (SELECT 
        rbs.northstar_id,
        sum(rbs.reportback_30) AS reportback_30,
        sum(rbs.reportback_60) AS reportback_60,
        sum(rbs.reportback_90) AS reportback_90
    FROM 
        (SELECT 
            c.northstar_id,
            c.signup_id,
            c.campaign_run_id,
            max(CASE WHEN c.post_id <> -1 AND c.status='accepted' AND c.submission_created_at >= '2017-08-01' AND c.submission_created_at <= '2017-08-31'  THEN 1 ELSE 0 END) AS reportback_30,
            max(CASE WHEN c.post_id <> -1 AND c.status='accepted' AND c.submission_created_at > '2017-08-31' AND c.submission_created_at <= '2017-09-30'  THEN 1 ELSE 0 END) AS reportback_60,
              max(CASE WHEN c.post_id <> -1 AND c.status='accepted' AND c.submission_created_at > '2017-09-30' AND c.submission_created_at <= '2017-10-31'  THEN 1 ELSE 0 END) AS reportback_90
        FROM quasar.campaign_activity c
        GROUP BY c.northstar_id, c.signup_id) rbs
    WHERE rbs.campaign_run_id <> 7890
    GROUP BY rbs.northstar_id) non_7890_rbs
    ON non_7890_rbs.northstar_id = u.northstar_id
LEFT JOIN 
     (SELECT 
         c1.northstar_id,
         c1.signup_id,
         sum(CASE WHEN c1.post_id <> -1 AND c1.status='accepted' AND c1.submission_created_at >= '2017-08-01' AND c1.submission_created_at <= '2017-08-31' THEN 1 ELSE 0 END) AS reportback_30,
         sum(CASE WHEN c1.post_id <> -1 AND c1.status='accepted' AND c1.submission_created_at > '2017-08-31' AND c1.submission_created_at <= '2017-09-30' THEN 1 ELSE 0 END) AS reportback_60,
         sum(CASE WHEN c1.post_id <> -1 AND c1.status='accepted' AND c1.submission_created_at > '2017-09-30' AND c1.submission_created_at <= '2017-10-31' THEN 1 ELSE 0 END) AS reportback_90
     FROM quasar.campaign_activity c1
     WHERE c1.campaign_run_id = 7890
     GROUP BY c1.northstar_id
     ) rbs_7890
ON u.northstar_id = rbs_7890.northstar_id
WHERE
u.northstar_id IN ('57cf6cf342a06413068b4592',