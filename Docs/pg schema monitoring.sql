-- Param uses these for periodic checks on the size of our database.
-- We have PG Hero but I find it easier to look at it this way because we can also identify what we can clean up
-- size of the db
SELECT pg_size_pretty( pg_database_size('quasar_prod_warehouse') );

-- size of each schema

SELECT table_schema
	, pg_size_pretty(sum(total_bytes)) AS total
    , pg_size_pretty(sum(index_bytes)) AS INDEX
    , pg_size_pretty(sum(toast_bytes)) AS toast
    , pg_size_pretty(sum(table_bytes)) AS table
    , sum(total_bytes)
  FROM (
  SELECT *, total_bytes-index_bytes-COALESCE(toast_bytes,0) AS table_bytes FROM (
      SELECT c.oid,nspname AS table_schema, relname AS TABLE_NAME
              , c.reltuples AS row_estimate
              , pg_total_relation_size(c.oid) AS total_bytes
              , pg_indexes_size(c.oid) AS index_bytes
              , pg_total_relation_size(reltoastrelid) AS toast_bytes
          FROM pg_class c
          LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
          WHERE relkind = 'r'
  ) a
) a
group by 1
order by 6 desc


-- table clean up etc
SELECT a.table_schema
	, a.table_name
	, pt.tableowner
	, a.row_estimate
	, pg_size_pretty(a.total_bytes) AS total
    , pg_size_pretty(a.index_bytes) AS index
    , pg_size_pretty(a.toast_bytes) AS toast
    , pg_size_pretty(a.table_bytes) AS table
    , a.total_bytes
  FROM
  	(SELECT *, total_bytes-index_bytes-COALESCE(toast_bytes,0) AS table_bytes
  		FROM
  		(SELECT c.oid,nspname AS table_schema, relname AS TABLE_NAME
	          , c.reltuples AS row_estimate
	          , pg_total_relation_size(c.oid) AS total_bytes
	          , pg_indexes_size(c.oid) AS index_bytes
	          , pg_total_relation_size(reltoastrelid) AS toast_bytes
	          FROM pg_class c
	          LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
	          WHERE relkind = 'r') a
	          	) a
	   left join pg_tables pt on a.table_schema = pt.schemaname and a.table_name = pt.tablename
	order by 1,9 desc

____________________

select pid, usename, application_name, state, wait_event_type query, backend_type, query
FROM pg_stat_activity
order by 2

--# of records
SELECT nspname AS schemaname,relname,reltuples
FROM pg_class C LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
WHERE
  nspname NOT IN ('pg_catalog', 'information_schema') and relkind='r' and nspname ='public' order by schemaname

-- size of the db
SELECT pg_size_pretty( pg_database_size('quasar_prod_warehouse') );

-- the size of the tables
SELECT pg_size_pretty( pg_total_relation_size('analyst_sandbox.pkg_user_daily_status') );

SELECT table_schema, table_name, table_catalog FROM information_schema.tables where table_catalog = 'quasar_prod_warehouse' and table_schema = 'public'
select schemaname, tablename,tableowner from pg_tables

SELECT
  nspname AS schemaname,relname,reltuples
FROM pg_class C
LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
WHERE
  nspname NOT IN ('pg_catalog', 'information_schema')
  and relkind='r'
  and nspname ='public'
 order by schemaname

 SELECT table_schema, table_name, table_catalog FROM information_schema.tables where table_catalog = 'quasar_prod_warehouse' and table_schema = 'public'
select schemaname, tablename,tableowner from pg_tables where tableowner = 'quasar_prod_admin' and schemaname = 'public' order by 2
