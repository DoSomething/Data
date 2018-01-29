DROP TABLE IF EXISTS public.phoenix_next_events;
DROP TABLE IF EXISTS public.phoenix_next_sessions;
DROP TABLE IF EXISTS public.device_northstar_crosswalk;

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

CREATE TABLE public.phoenix_next_events AS 
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
	
CREATE TABLE public.phoenix_next_sessions_temp AS 
	(SELECT DISTINCT
			page.sessionid_s AS session_id,  
			use.deviceid_s AS device_id,
			use.ip_s AS ip_address,
			COALESCE(page.landingtimestamp_d, 
				(CASE WHEN page.landingtimestamp_s = 'null' THEN NULL ELSE page.landingtimestamp_s END)::bigint
				)::bigint AS landing_ts,
			refer.path_s AS referrer_path,
			refer.host_s AS referrer_host,
			refer.href_s AS referrer_href,
			ref_q.from_session_s,
			ref_q.source_s AS referrer_source,
			ref_q.utm_medium_s AS referrer_utm_medium,
			ref_q.utm_source_s AS referrer_utm_source,
			ref_q.utm_campaign_s AS referrer_utm_campaign
		FROM heroku_wzsf6b3z.events_page page
		LEFT JOIN heroku_wzsf6b3z.events_user use ON page.did = use.did
		LEFT JOIN heroku_wzsf6b3z.events_page_referrer refer ON refer.did = page.did
		LEFT JOIN heroku_wzsf6b3z.events_page_referrer_query ref_q ON ref_q.did = page.did)
;

CREATE TABLE public.device_northstar_crosswalk AS 
	(SELECT 
		nsids.deviceid_s,
		nsids.northstarid_s,
		counts.proportion
	FROM 
		(SELECT 
			dis.deviceid_s,
			1/count(dis.deviceid_s)::FLOAT AS proportion
		FROM 
			(SELECT DISTINCT 
			    u.deviceid_s,
			    u.northstarid_s
			  FROM heroku_wzsf6b3z.events_user u 
			  WHERE u.northstarid_s IS NOT NULL) dis
		GROUP BY dis.deviceid_s) counts
	LEFT JOIN 
		(SELECT DISTINCT 
		    u.deviceid_s,
		    u.northstarid_s
		 FROM heroku_wzsf6b3z.events_user u 
		 WHERE u.northstarid_s IS NOT NULL) nsids
		ON nsids.deviceid_s = counts.deviceid_s
	);

GRANT SELECT ON public.phoenix_next_sessions TO jjensen;
GRANT SELECT ON public.phoenix_next_sessions TO public;
GRANT SELECT ON public.phoenix_next_sessions TO looker;
GRANT SELECT ON public.phoenix_next_events TO public;
GRANT SELECT ON public.device_northstar_crosswalk TO public;
GRANT SELECT ON ALL tables IN SCHEMA public TO jjensen;
GRANT SELECT ON ALL tables IN SCHEMA public TO looker;
GRANT SELECT ON ALL tables IN SCHEMA public TO shasan;
GRANT SELECT ON ALL tables IN SCHEMA public TO quasaradm;
GRANT SELECT ON ALL tables IN SCHEMA heroku_wzsf6b3z TO jjensen;
GRANT SELECT ON ALL tables IN SCHEMA heroku_wzsf6b3z TO shasan;
GRANT SELECT ON ALL tables IN SCHEMA heroku_wzsf6b3z TO looker;
GRANT SELECT ON ALL tables IN SCHEMA heroku_wzsf6b3z TO quasaradm;