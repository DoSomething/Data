DROP MATERIALIZED VIEW IF EXISTS engaged_niche;
CREATE MATERIALIZED VIEW IF NOT EXISTS engaged_niche AS (
SELECT
  distinct a.northstar_id
  , a.created_at
  , a.event_name
  , a.timestamp
FROM (
 -- web activation 
    SELECT
      u.northstar_id AS 'northstar_id'
      , u.created_at AS 'created_at'
      , 'activated' AS 'event_name'
      , l.last_logged_in AS 'timestamp'
    FROM
      quasar.users u
    LEFT JOIN
      quasar.users_log l
      ON
      l.northstar_id = u.northstar_id
    WHERE
      u.source = 'niche'
      AND l.last_logged_in <> '0000-00-00 00:00:00'
      AND l.last_logged_in <> '1970-01-01 00:00:00'
    GROUP BY
      u.northstar_id
    HAVING
      min(l.last_logged_in) > u.created_at
  UNION ALL
-- sms signups
    SELECT
      u.northstar_id AS 'northstar_id'
      , u.created_at AS 'created_at'
      , 'sms_signup' AS 'event_name'
      , MAX(signup_created_at) AS 'timestamp'
    FROM
      quasar.users u
    LEFT JOIN
      quasar.campaign_activity c
      ON
      c.northstar_id = u.northstar_id
    LEFT JOIN
      quasar.users_log l
      ON
      l.northstar_id = u.northstar_id
    WHERE
      u.source = 'niche'
    GROUP BY
      u.northstar_id
    HAVING
      count(distinct c.signup_id) > 1
      AND min(l.last_logged_in) <= u.created_at
    ) AS a
GROUP BY
  a.northstar_id);
GRANT SELECT ON engaged_niche TO looker;
GRANT SELECT ON engaged_niche TO jjensen;
GRANT SELECT ON engaged_niche TO jli;
GRANT SELECT ON engaged_niche TO shasan;