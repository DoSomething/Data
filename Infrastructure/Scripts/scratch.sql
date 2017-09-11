SELECT COLUMN_NAME
FROM information_schema.columns c
WHERE c.TABLE_SCHEMA='quasar'
AND table_name='users';

SELECT * FROM users_and_activities.mobile_master_lookup l
WHERE l.
limit 10;


SELECT
    u.northstar_id,
    u.email,
    u.northstar_created_at_timestamp as created_date,
    u.northstar_id_source_name
  FROM quasar.users u
  WHERE u.northstar_created_at_timestamp > '2017-01-01'
LIMIT 50