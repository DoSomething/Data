 SELECT
        a.date as "date"
        , a.campaign_run_id as "campaign_run_id"
        , a.rogue_rbs as "photo_rbs"
        , a.text_rbs as "text_rbs"
        , a.rbs as "rbs"
        , a.calls as "calls"
        , a.social as "social"
        , a.voter_registrations as "voter_registrations"
        , a.self_reported_registrations as "self_reported_registrations"
        , a.other as "other"
      FROM (
        SELECT
          rogue.date as "date",
          rogue.campaign_run_id as "campaign_run_id",
          rogue.rogue_rbs as "rogue_rbs",
          rogue.text_rbs as "text_rbs",
          0 as "rbs",
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
              COUNT(DISTINCT(CASE WHEN ca.post_type = 'voter-reg' AND ca.post_status = 'confirmed' THEN ca.signup_id END)) as "self_reported_registrations",
              COUNT(DISTINCT(CASE WHEN ca.post_type = 'voter-reg' AND ca.post_status ILIKE '%register%' THEN ca.signup_id END)) as "voter_registrations",
              ca.campaign_run_id AS "campaign_run_id",
             date_trunc('month',ca.post_created_at) as post_created_month
            FROM
              public.campaign_activity ca
            WHERE
              ca.post_id > 0
            AND
              (ca.post_type <> 'voter-reg' OR ca.post_type IS NULL)
            AND
              ca.post_status <> 'rejected'
            GROUP BY
              post_created_month, ca.campaign_run_id
            )
            AS rogue
      UNION ALL
        SELECT
          l.date AS "date",
          l.campaign_run_id as "campaign_run_id",
          0 as "rogue_rbs",
          0 as "text_rbs",
          l.rbs AS "rbs",
          l.calls AS "calls",
          l.social AS "social",
          0 AS "voter_registrations",
          0 AS "self_reported_registrations",
          l.other AS "other"
        FROM
          public.legacy_reportbacks l
          ) as a
      ORDER BY a.date DESC