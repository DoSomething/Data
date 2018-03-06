SELECT
  u.created_at,
  mel.event_id,
  mel.timestamp,
  mel.northstar_id,
  mel.action_type
FROM quasar.users u
INNER JOIN quasar.member_event_log mel ON mel.northstar_id = u.northstar_id
WHERE
(mel.timestamp >= '2014-01-01' OR u.created_at > '2008-01-01')
AND mel.timestamp <= 'TODAY_DATE';
