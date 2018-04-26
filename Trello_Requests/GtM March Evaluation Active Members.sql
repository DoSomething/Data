/*Pull members who were active in March, opened an email since March and signed up for Grab the Mic*/
SELECT DISTINCT e.northstar_id,
min(mel.timestamp) as 'active_in_march'
FROM cio.event_log e 
INNER JOIN quasar.member_event_log mel
ON e.northstar_id=mel.northstar_id
INNER JOIN quasar.campaign_activity ca
ON e.northstar_id=ca.northstar_id
INNER JOIN cio.customer_event ce
ON e.northstar_id=ce.customer_id
WHERE ca.campaign_run_id = 8022 AND mel.timestamp >= '2018-03-01' AND mel.timestamp < '2018-04-01' AND JSON_EXTRACT(e.`data`, "$.event_type") IN ('email_opened') AND ce.timestamp >='2018-03-01'
GROUP BY e.northstar_id;