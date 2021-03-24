SELECT
	p.campaign_data #>> '{data,id}' AS contentful_id,
	p.campaign_data #>> '{data,legacyCampaignId}' AS campaign_node_id,
	p.campaign_data #>> '{data,title}' AS campaign_node_id_title,
	p.campaign_data #>> '{data,legacyCampaignRunId}' AS campaign_run_id,
	p.campaign_data #>> '{data,title}' AS campaign_run_id_title,
	p.campaign_data #>> '{data,type}' AS campaign_type,
	--campaign_language,
	--campaign_run_start_date,
	(p.campaign_data #> '{data,endDate}' ->> 'date')::TIMESTAMP AS campaign_run_end_date,
	--campaign_created_date,
	p.campaign_data #> '{data,additionalContent}' #>> '{noun,plural}' AS campaign_noun,
	p.campaign_data #> '{data,additionalContent}' #>> '{verb,plural}' AS campaign_verb,
	p.campaign_data #>> '{data,cause}' AS campaign_cause_type,
	p.campaign_data #>> '{data,callToAction}' AS campaign_cta
	--campaign_action_type
FROM phoenix.campaigns_json p
;
