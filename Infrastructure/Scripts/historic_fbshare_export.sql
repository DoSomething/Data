SELECT
	e.records #>> '{_id,$oid}' AS _id,
	e.records #>> '{meta,id}' AS "meta.id",
	(e.records #>> '{meta,timestamp}')::bigint AS "meta.timestamp",
	to_timestamp((e.records #>> '{meta,timestamp}')::bigint/1000) AS "to_timestamp",
	e.records #>> '{event,name}' AS "event.name",
	e.records #>> '{event,source}' AS "event.source",
	e.records #>> '{page,path}' AS "page.path",
	e.records #>> '{page,host}' AS "page.host",
	e.records #>> '{page,href}' AS "page.href",
	COALESCE(e.records #>> '{data,legacyCampaignId}',p.campaign_id) AS "campaign_id",
	NULL as "data.legacyCampaignId",
	'share-social' AS "type",
	e.records #> '{page,query}' ->> 'utm.source' AS "page.utm.source",
	e.records #> '{page,query}' ->> 'utm.medium' AS "page.utm.medium",
	e.records #> '{page,query}' ->> 'utm.campaign' AS "page.utm.campaign",
	e.records #>> '{data,parentSource}' AS "data.parentSource",
	e.records #>> '{data,campaignId}' "data.campaignId",
	e.records #>> '{data,url}' "data.url",
	e.records #>> '{data,source}' AS "data.source",
	e.records #>> '{data,link}' AS "data.link",
	e.records #>> '{data,modalType}' AS "data.modalType",
	e.records #>> '{data,variant}' AS "data.variant",
	e.records #> '{data,sourceData}' ->> 'text' AS "data.sourceData.text",
	e.records #>> '{page,sessionId}' AS "page.sessionId",
	e.records #>> '{browser,size}' AS "browser.size",
	e.records #>> '{user,northstarId}' AS "user.northstarId"
FROM puck.events_json e
LEFT JOIN public.phoenix_events p ON e.records #>> '{_id,$oid}' = p.event_id
WHERE p.event_name in ('share action completed','facebook share posted')
-- AND to_timestamp((e.records #>> '{meta,timestamp}')::bigint/1000) < '2018-08-09 19:31:59'
AND (e.records #>> '{meta,timestamp}')::bigint >= 1545149040000
AND (e.records #>> '{meta,timestamp}')::bigint < 1546551180000
ORDER BY (e.records #>> '{meta,timestamp}')::bigint DESC ;

