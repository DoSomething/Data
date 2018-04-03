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


