DROP TABLE IF EXISTS public.phoenix_next_events;
DROP TABLE IF EXISTS public.phoenix_next_sessions;
DROP TABLE IF EXISTS public.phoenix_next_signup_data;

SELECT 
	meta.did AS event_id,
	meta.timestamp_d AS ts,
	event.name_s AS event_name,
	event.source_s AS event_source,
	page.path_s AS path,
	page.host_s AS host,
	page.href_s AS href, 
	page.sessionid_s AS session_id,
	use.northstarid_s,
	brow.size_s AS device_size
INTO public.phoenix_next_events
FROM heroku_wzsf6b3z.events_meta meta
LEFT JOIN heroku_wzsf6b3z.events_event event ON event.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_page page ON page.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_user use ON use.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_browser brow ON brow.did = meta.did 
;

SELECT 
	meta.did AS event_id,
	dat.parentsource_s AS parent_source, 
	dat.campaignid_s AS campaign_id,
	dat.source_s AS source,
	dat.link_s AS link,
	dat.modaltype_s AS modal_type,
	dat.variant_s AS variant,
	sdata.text_s AS source_data_text
INTO public.phoenix_next_signup_data
FROM heroku_wzsf6b3z.events_meta meta
LEFT JOIN heroku_wzsf6b3z.events_event event ON event.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_data dat ON dat.did = meta.did
LEFT JOIN heroku_wzsf6b3z.events_data_sourcedata sdata ON sdata.did = meta.did
WHERE event.name_s = 'signup'
;

SELECT DISTINCT
	page.sessionid_s AS session_id,  
	use.deviceid_s AS device_id,
	COALESCE(page.landingtimestamp_d, page.landingtimestamp_s::numeric) AS landing_ts,
	refer.path_s AS referrer_path,
	refer.host_s AS referrer_host,
	refer.href_s AS referrer_href,
	ref_q.from_session_s,
	ref_q.source_s AS referrer_source,
	ref_q.utm_medium_s AS referrer_utm_medium,
	ref_q.utm_source_s AS referrer_utm_source,
	ref_q.utm_campaign_s AS referrer_utm_campaign
INTO phoenix_next_sessions
FROM heroku_wzsf6b3z.events_page page
LEFT JOIN heroku_wzsf6b3z.events_user use ON page.did = use.did
LEFT JOIN heroku_wzsf6b3z.events_page_referrer refer ON refer.did = page.did
LEFT JOIN heroku_wzsf6b3z.events_page_referrer_query ref_q ON ref_q.did = page.did;

GRANT SELECT ON phoenix_next_sessions TO public;
GRANT SELECT ON phoenix_next_signup_data TO public;
GRANT SELECT ON phoenix_next_events TO public;