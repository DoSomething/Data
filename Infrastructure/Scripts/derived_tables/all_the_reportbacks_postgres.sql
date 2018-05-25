 SELECT
        a.date AS "date"
        , a.campaign_run_id AS "campaign_run_id"
        , sum(a.rogue_rbs) AS "photo_rbs"
        , sum(a.text_rbs) AS "text_rbs"
        , sum(a.rbs) AS "rbs"
        , sum(a.calls) AS "calls"
        , sum(a.social) AS "social"
        , sum(a.voter_registrations) AS "voter_registrations"
        , sum(a.self_reported_registrations) AS "self_reported_registrations"
        , sum(a.other) AS "other"
      FROM (
        SELECT
          rogue.post_created_month AS "date",
          rogue.campaign_run_id AS "campaign_run_id",
          rogue.rogue_rbs AS "rogue_rbs",
          rogue.text_rbs AS "text_rbs",
          0 AS "rbs",
          0 AS "calls",
          0 AS "social",
          rogue.voter_registrations AS "voter_registrations",
          rogue.self_reported_registrations AS "self_reported_registrations",
          0 AS "other"
        FROM
            (
            SELECT
              COUNT(DISTINCT(CASE WHEN (ca.post_type = 'photo' or ca.post_type IS NULL) THEN ca.signup_id END)) AS "rogue_rbs",
              COUNT(DISTINCT(CASE WHEN ca.post_type = 'text' THEN ca.signup_id END)) AS "text_rbs",
             -- COUNT(DISTINCT(CASE WHEN ca.post_type = 'voter-reg' AND ca.post_status = 'confirmed' THEN ca.signup_id END)) AS "self_reported_registrations",
             -- COUNT(DISTINCT(CASE WHEN ca.post_type = 'voter-reg' AND ca.post_status ILIKE '%register%' THEN ca.signup_id END)) AS "voter_registrations",
              0 AS "self_reported_registrations",
              0 AS "voter_registrations",
              ca.campaign_run_id AS "campaign_run_id",
             date_trunc('month',ca.post_created_at) AS post_created_month
            FROM
              public.campaign_activity ca
            WHERE
              ca.post_id IS NOT NULL AND ca.post_id > 0
            AND
              ca.post_status IS DISTINCT FROM 'rejected'
            GROUP BY
              date_trunc('month',ca.post_created_at), ca.campaign_run_id
            )
            AS rogue
      UNION ALL
        SELECT
          l.date AS "date",
          l.campaign_run_id AS "campaign_run_id",
          0 AS "rogue_rbs",
          0 AS "text_rbs",
          l.rbs AS "rbs",
          l.calls AS "calls",
          l.social AS "social",
          l.voter_registrations AS "voter_registrations",
          l.self_reported_registrations AS "self_reported_registrations",
          l.other AS "other"
        FROM
          public.legacy_reportbacks l
          ) AS a
      GROUP BY a."date", a.campaign_run_id
      ORDER BY a."date" DESC