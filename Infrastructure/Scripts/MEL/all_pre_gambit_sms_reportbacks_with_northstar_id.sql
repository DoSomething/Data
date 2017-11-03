SELECT 
	temp.uid AS 'drupal_id',
	temp.field_northstar_id_value AS 'northstar_id',
		temp.nid AS 'campaign_node_id',
	temp.run_nid AS 'campaign_run_id',
	temp.created AS 'submission_created_at_timestamp'

FROM 
	(SELECT DISTINCT 
		rb.uid AS 'uid',
		ns.field_northstar_id_value,
		rb.nid,
		rb.run_nid,
		rb.created,
		'sms' AS 'post_source'
	FROM dosomething.dosomething_reportback rb
	LEFT JOIN 
		dosomething.dosomething_signup ds
		ON rb.uid = ds.uid AND rb.run_nid = ds.run_nid
	INNER JOIN
		field_data_field_northstar_id ns
		ON ns.entity_id = rb.uid
	WHERE 
		ds.sid IS NULL 
		) temp
GROUP BY temp.uid, temp.RUN_NID