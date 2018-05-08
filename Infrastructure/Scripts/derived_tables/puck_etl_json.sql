CREATE TEMPORARY TABLE VIEW path_campaign_lookup AS 
	(
	SELECT 
		max(camps.campaign_id) AS campaign_id,
		camps.campaign_name
	FROM 
		(SELECT DISTINCT 
			COALESCE(
				NULLIF(regexp_replace(e.records #>> '{data,campaignId}', '[^0-9.]','','g'), ''),
				NULLIF(regexp_replace(e.records #>> '{data,legacyCampaignId}', '[^0-9.]','','g'), '')
		 		) AS campaign_id,
			(regexp_split_to_array(e.records #>> '{page,path}', E'\/'))[4] AS campaign_name
			FROM puck.events e
			WHERE e.records #>> '{data,campaignId}' IS NOT NULL 
				OR e.records #>> '{data,legacyCampaignId}' IS NOT NULL 
			) camps
	GROUP BY camps.campaign_name
	)
;
CREATE TEMPORARY TABLE puck_events_test AS (
	SELECT 
		e.records #>> '{_id,$oid}' AS object_id,
		e.records #>> '{meta,id}' AS puck_id,
		to_timestamp((e.records #>> '{meta,timestamp}')::bigint/1000) AS event_datetime,
		e.records #>> '{meta,timestamp}' AS ts,
		e.records #>> '{event,name}' AS event_name,
		e.records #>> '{event,source}' AS event_source,
		e.records #>> '{page,path}' AS "path",
		e.records #>> '{page,host}' AS host,
		e.records #>> '{page,href}' AS href,
		e.records #> '{page,query}' ->> 'utm_source' AS page_utm_source,
		e.records #> '{page,query}' ->> 'utm_medium' AS page_utm_medium,
		e.records #> '{page,query}' ->> 'utm_campaign' AS page_utm_campaign,
		e.records #>> '{data,parentSource}' AS parent_source,
		COALESCE(dat.campaign_id::varchar, lookup.campaign_id::varchar) AS campaign_id,
		CASE WHEN e.records #>> '{page,href}' ILIKE '%password/reset%' THEN NULL ELSE page.campaign_name AS campaign_name,
		e.records #>> '{data,source}' AS "source",
		e.records #>> '{data,link}' AS link,
		e.records #>> '{data,modalType}' AS modal_type,
		e.records #>> '{data,variant}' AS variant,
		e.records #> '{data,sourceData}' ->> 'text' AS source_data_text,
		e.records #>> '{page,sessionId}' AS session_id,
		e.records #>> '{browser,size}' AS browser_size,
		e.records #>> '{user,northstarId}' AS northstar_id
	FROM puck.events e
	LEFT JOIN 
		(SELECT 
			edat.records #>> '{_id,$oid}' AS object_id,
			COALESCE(
				NULLIF(regexp_replace(edat.records #>> '{data,legacyCampaignId}', '[^0-9.]','','g'), ''),
				NULLIF(regexp_replace(edat.records #>> '{data,campaignId}', '[^0-9.]','','g'), '')
		 		) AS campaign_id
		FROM puck.events edat
		WHERE edat.records #> '{data}' IS NOT NULL) dat ON e.records #>> '{_id,$oid}' = dat.object_id
	LEFT JOIN 
		(SELECT 
			p.records #>> '{_id,$oid}' AS object_id,
			(regexp_split_to_array(p.records #>> '{page,path}', E'\/'))[4] AS campaign_name 
		FROM puck.events p) page ON page.object_id = e.records #>> '{_id,$oid}'
	LEFT JOIN path_campaign_lookup lookup ON page.campaign_name = lookup.campaign_name
	WHERE to_timestamp((e.records #>> '{meta,timestamp}')::bigint/1000) >= '2018-02-01'
) 
;
DROP TABLE IF EXISTS puck_events_test;
SELECT * FROM puck_events_test t WHERE t.campaign_name IS NOT NULL;
SELECT e.records #> '{data}' FROM puck.events e
WHERE e.records #>> '{data}' ILIKE '%campaign%'  ;
SELECT * FROM puck.events e WHERE e.records #>> '{_id,$oid}' = '5a72802e8428fb00046906d8'