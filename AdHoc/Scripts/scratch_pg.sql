SELECT * 
FROM events_test
WHERE event_name = 'view'
AND PATH LIKE '%missing%'
AND campaign_id IS NULL;

DROP TABLE IF EXISTS path_campaign_lookup;

CREATE TEMPORARY TABLE path_campaign_lookup AS 
	(
	SELECT 
		max(camps.campaign_id) AS campaign_id,
		camps.campaign_name
	FROM 
		(SELECT DISTINCT 
			dat.campaignid_s::NUMERIC AS campaign_id,
			(regexp_split_to_array(page.path_s, E'\/'))[4] AS campaign_name
			FROM heroku_wzsf6b3z.events_meta meta
			LEFT JOIN heroku_wzsf6b3z.events_data dat ON dat.did = meta.did
			LEFT JOIN heroku_wzsf6b3z.events_page page ON page.did = meta.did
			WHERE dat.campaignid_s IS NOT NULL
			) camps
	GROUP BY camps.campaign_name
	)
;
SELECT * FROM path_campaign_lookup 
/*WHERE full_path = '/us/campaigns/missing-in-history/modal/72lg25OHCgKuUQkuKK68mg'*/ 
LIMIT 500;

SELECT 
(regexp_split_to_array(page.path_s, E'\/'))[4] AS path_array
FROM heroku_wzsf6b3z.events_page page
LIMIT 50;

DROP TABLE IF EXISTS events_test;
CREATE TEMPORARY TABLE events_test AS 
	(SELECT 
		CASE WHEN meta.id_s IS NULL THEN meta.did::VARCHAR ELSE meta.id_s END AS event_id,
		meta.timestamp_d AS ts,
		event.name_s AS event_name,
		event.source_s AS event_source,
		page.path AS path,
		page.host AS host,
		page.href AS href, 
		dat.parentsource_s AS parent_source, 
		COALESCE(dat.campaignid_s::varchar, lookup.campaign_id::varchar) AS campaign_id,
		page.campaign_name,
		dat.source_s AS source,
		dat.link_s AS link,
		dat.modaltype_s AS modal_type,
		dat.variant_s AS variant,
		sdata.text_s AS source_data_text,
		page.session_id AS session_id,
		use.northstarid_s,
		brow.size_s AS device_size
	FROM heroku_wzsf6b3z.events_meta meta
	LEFT JOIN heroku_wzsf6b3z.events_event event ON event.did = meta.did
	LEFT JOIN heroku_wzsf6b3z.events_data dat ON dat.did = meta.did
	LEFT JOIN heroku_wzsf6b3z.events_data_sourcedata sdata ON sdata.did = meta.did
	LEFT JOIN 
		(SELECT 
			p.did,
			p.path_s AS path,
			p.host_s AS host,
			p.href_s AS href, 
			p.sessionid_s AS session_id,
			(regexp_split_to_array(p.path_s, E'\/'))[4] AS campaign_name
		FROM heroku_wzsf6b3z.events_page p) page ON page.did = meta.did
	LEFT JOIN path_campaign_lookup lookup ON page.campaign_name = lookup.campaign_name
	LEFT JOIN heroku_wzsf6b3z.events_user use ON use.did = meta.did
	LEFT JOIN heroku_wzsf6b3z.events_browser brow ON brow.did = meta.did
	)
;
SELECT DISTINCT t.campaign_name
FROM events_test t 
LEFT JOIN path_campaign_lookup l ON l.campaign_name = t.campaign_name
WHERE t.campaign_id IS NULL 
AND t.path ILIKE '%campaigns%' 
LIMIT 200;

SELECT * FROM public.phoenix_next_sessions LIMIT 5;

SELECT * , (regexp_split_to_array(page.path_s, E'\/'))[4] AS campaign_name
FROM heroku_wzsf6b3z.events_meta meta
LEFT JOIN heroku_wzsf6b3z.events_data dat ON dat.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_page page ON page.did = meta.did
WHERE dat.campaignid_s IS NOT NULL
AND page.path_s ILIKE '%mirror%'

SELECT max(to_timestamp(ts/1000)) FROM public.phoenix_next_events limit 10;

SELECT  FROM pg_catalog.pg_stat_activity LIMIT 100;
SELECT * FROM pg_catalog.pg_user LIMIT 100;
GRANT SELECT ON public.phoenix_next_sessions TO rdsadmin;

SELECT 
count(*),
count(DISTINCT event_id)
FROM phoenix_next_events 
--WHERE to_timestamp(ts/1000) > '2017-12-30'
LIMIT 100;

SELECT 
to_timestamp(ts/1000) AS datetime,
*
FROM phoenix_next_events e
INNER JOIN (
	SELECT 
	ev.event_id,
	count(ev.*)
	FROM public.phoenix_next_events ev
	GROUP BY ev.event_id
	HAVING count(*) > 1
	) dup ON dup.event_id = e.event_id  
ORDER BY e.event_id, to_timestamp(e.ts/1000);

SELECT e.ts FROM public.phoenix_next_events e LIMIT 100;
SELECT * FROM public.phoenix_next_sessions WHERE session_id='1515613095723360801515613095724';


SELECT 
count(*),
count(DISTINCT session_id)
FROM phoenix_next_sessions 
WHERE to_timestamp(ts/1000) > '2017-12-30'
LIMIT 100;

SELECT * FROM phoenix_next_events WHERE to_timestamp(ts/1000) > '2017-12-30';
SELECT * FROM public.campaign_info_postgres i WHERE i.campaign_node_id_title LIKE '%mascot%';


<<<<<<< HEAD
=======
SELECT DISTINCT e.event_type FROM cio.customer_event e ;
SELECT * FROM northstar.users LIMIT 100;
SELECT 
	substring(u.created_at::varchar,1,7),
	count(*)
FROM northstar.users u
GROUP BY substring(u.created_at::varchar,1,7)
ORDER BY substring(u.created_at::varchar,1,7);
>>>>>>> master;
SELECT * FROM phoenix_next_sessions;

SELECT 
	meta.did AS event_id,
	page_q.did AS pq_eid,
	to_timestamp(meta.timestamp_d/1000) AS "timestamp"
FROM heroku_wzsf6b3z.events_meta meta
LEFT JOIN heroku_wzsf6b3z.events_page_query page_q ON page_q.did = meta.did
WHERE to_timestamp(meta.timestamp_d/1000) >= '2018-05-02'
AND page_q.did IS NOT NULL ;

SELECT to_timestamp(m.timestamp_d/1000) AS "timestamp" FROM heroku_wzsf6b3z.events_meta m WHERE m.did = 1344187157;


SELECT DISTINCT
	meta.did,
	event.name_s,
	page.sessionid_s AS session_id,  
	use.deviceid_s AS device_id,
	COALESCE(page.landingtimestamp_d, 
		(CASE WHEN page.landingtimestamp_s = 'null' THEN NULL ELSE page.landingtimestamp_s END)::bigint
		)::bigint AS landing_ts
FROM heroku_wzsf6b3z.events_meta meta
LEFT JOIN heroku_wzsf6b3z.events_event event ON event.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_page page ON meta.did = page.did
LEFT JOIN heroku_wzsf6b3z.events_user use ON page.did = use.did
ORDER BY page.sessionid_s
LIMIT 100;

		SELECT
			page.sessionid_s AS session_id,  
			max(use.deviceid_s) AS device_id,
			min(COALESCE(page.landingtimestamp_d, 
				(CASE WHEN page.landingtimestamp_s = 'null' THEN NULL ELSE page.landingtimestamp_s END)::bigint
				)::bigint) AS landing_ts,
			max(refer.path_s) AS referrer_path,
			max(refer.host_s) AS referrer_host,
			max(refer.href_s) AS referrer_href,		
			max(ref_q.from_session_s),
			max(ref_q.source_s) AS referrer_source,
			max(ref_q.utm_medium_s) AS referrer_utm_medium,
			max(ref_q.utm_source_s) AS referrer_utm_source,
			max(ref_q.utm_campaign_s) AS referrer_utm_campaign
		FROM heroku_wzsf6b3z.events_page page
		LEFT JOIN 
			(SELECT 
				page_temp.sessionid_s,
				max(use_temp.deviceid_s::bigint) AS deviceid_s
			FROM heroku_wzsf6b3z.events_page page_temp
			LEFT JOIN heroku_wzsf6b3z.events_user use_temp ON page_temp.did = use_temp.did
			GROUP BY page_temp.sessionid_s) use ON page.sessionid_s = use.sessionid_s
		LEFT JOIN heroku_wzsf6b3z.events_page_referrer refer ON refer.did = page.did
		LEFT JOIN heroku_wzsf6b3z.events_page_referrer_query ref_q ON ref_q.did = page.did
		GROUP BY page.sessionid_s;
			
SELECT 
	page.sessionid_s AS session_id,  
	max(refer.path_s) AS referrer_path,
	max(refer.host_s) AS referrer_host,
	max(refer.href_s) AS referrer_href 
FROM heroku_wzsf6b3z.events_page page
LEFT JOIN (SELECT 
				refer_temp.did,
				refer_temp.path_s,
				refer_temp.host_s,
				refer_temp.href_s
			FROM heroku_wzsf6b3z.events_page_referrer refer_temp 
			WHERE refer_temp.path_s IS NOT NULL) refer ON refer.did = page.did
WHERE page.sessionid_s = '1234483220159126651234483220159'
GROUP BY page.sessionid_s;

SELECT 
	u.id,
	count(*) AS n_appearances
FROM northstar.users u 
GROUP BY u.id
ORDER BY count(*) DESC;

		SELECT DISTINCT 
			u.northstar_id,
			u.timestamp,
			'site_access' AS action,
			'3' AS action_id,
			NULL AS source,
			'0' AS action_serial_id,
			u.im_from
		FROM 
			(SELECT -- SITE ACCESS
				DISTINCT u_new.id AS northstar_id,
				u_new.last_accessed_at AS timestamp,
				'new' AS im_from 
			FROM
				northstar.users u_new
			WHERE u_new.last_accessed_at IS NOT NULL 
			UNION ALL 
			SELECT --SITE ACCESS LEGACY
				DISTINCT u_leg.northstar_id,
				u_leg.last_accessed AS timestamp,
				'legacy' AS im_from
			FROM 
				northstar.users_log_mysql u_leg
			WHERE u_leg.last_accessed IS NOT NULL 
				LIMIT 10000) u;
				

SELECT * FROM member_event_log m WHERE m.action_type = 'site_access' AND m.timestamp >= '2017-01-01' AND m."timestamp" < '2017-02-02';
SELECT * FROM northstar.users_log_mysql m WHERE m.last_accessed < '2017-01-02' AND m.last_accessed >= '2017-01-01'