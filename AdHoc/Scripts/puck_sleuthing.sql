SELECT 
		e.records #>> '{_id,$oid}' AS _id,
		e.records #>> '{meta,id}' AS "meta.id",
		(e.records #>> '{meta,timestamp}')::bigint AS "meta.timestamp",
		e.records #>> '{event,name}' AS "event.name",
		e.records #>> '{event,source}' AS "event.source",
		e.records #>> '{page,path}' AS "page.path",
		e.records #>> '{page,host}' AS "page.host",
		e.records #>> '{page,href}' AS "page.href",
		e.records #> '{page,query}' ->> 'utm.source' AS "page.utm.source",
		e.records #> '{page,query}' ->> 'utm.medium' AS "page.utm.medium",
		e.records #> '{page,query}' ->> 'utm.campaign' AS "page.utm.campaign",
		e.records #>> '{data,parentSource}' AS "data.parentSource",
		e.records #>> '{data,legacyCampaignId}' "data.legacyCampaignId",
		e.records #>> '{data,campaignId}' "data.campaignId",
		e.records #>> '{data,url}' "data.url",
		e.records #>> '{page,href}' "page.href",
		e.records #>> '{data,source}' AS "data.source",
		e.records #>> '{data,link}' AS "data.link",
		e.records #>> '{data,modalType}' AS "data.modalType",
		e.records #>> '{data,variant}' AS "data.variant",
		e.records #> '{data,sourceData}' ->> 'text' AS "data.sourceData.text",
		e.records #>> '{page,sessionId}' AS "page.sessionId",
		e.records #>> '{browser,size}' AS "browser.size",
		e.records #>> '{user,northstarId}' AS "user.northstarId"
	FROM puck.events_json e
	WHERE e.records #>> '{event,name}' = 'share action completed'
	ORDER BY (e.records #>> '{meta,timestamp}')::bigint DESC ;
	
SELECT 
		e.records,
		e.records #>> '{_id,$oid}' AS _id,
		e.records #>> '{meta,id}' AS "meta.id",
		(e.records #>> '{meta,timestamp}')::bigint AS "meta.timestamp",
		e.records #>> '{event,name}' AS "event.name",
		e.records #>> '{event,source}' AS "event.source",
		e.records #>> '{page,path}' AS "page.path",
		e.records #>> '{page,host}' AS "page.host",
		e.records #>> '{page,href}' AS "page.href",
		e.records #> '{page,query}' ->> 'utm.source' AS "page.utm.source",
		e.records #> '{page,query}' ->> 'utm.medium' AS "page.utm.medium",
		e.records #> '{page,query}' ->> 'utm.campaign' AS "page.utm.campaign",
		e.records #>> '{data,parentSource}' AS "data.parentSource",
		e.records #>> '{data,legacyCampaignId}' "data.legacyCampaignId",
		e.records #>> '{data,campaignId}' "data.campaignId",
		e.records #>> '{data,url}' "data.url",
		e.records #>> '{page,href}' "page.href",
		e.records #>> '{data,source}' AS "data.source",
		e.records #>> '{data,link}' AS "data.link",
		e.records #>> '{data,modalType}' AS "data.modalType",
		e.records #>> '{data,variant}' AS "data.variant",
		e.records #> '{data,sourceData}' ->> 'text' AS "data.sourceData.text",
		e.records #>> '{page,sessionId}' AS "page.sessionId",
		e.records #>> '{browser,size}' AS "browser.size",
		e.records #>> '{user,northstarId}' AS "user.northstarId"
	FROM puck.events_json e
	WHERE e.records #>> '{page,href}' ILIKE '%brake%'
	ORDER BY (e.records #>> '{meta,timestamp}')::bigint DESC ;
	
SELECT DISTINCT e.records #>> '{event,name}' FROM puck.events_json e WHERE e.records #>> '{data,legacyCampaignId}' IS NOT NULL OR e.records #>> '{data,campaignId}' IS NOT NULL ;

SELECT DISTINCT e.event_name FROM phoenix_events e WHERE e.href ILIKE '%brake%'