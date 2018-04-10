--Customer Event
SELECT 
	l."data"::json#>>'{data,email_id}' AS email_id,
	l."data"::json#>>'{data,customer_id}' AS customer_id,
	l."data"::json#>>'{data,email_address}' AS email_address,
	l."data"::json#>>'{data,template_id}' AS template_id,
	l.event_id,
	to_timestamp((l."data"::json->>'timestamp')::bigint) AS timestamp,
	l."data"::json->>'event_type' AS event_type
FROM cio_mysql.event_log l
WHERE l."data"::json->>'event_type' IN 
	('customer_subscribed', 
	'customer_unsubscribed');

--Email Event
SELECT 
	l."data"::json#>>'{data,email_id}' AS email_id,
	l."data"::json#>>'{data,customer_id}' AS customer_id,
	l."data"::json#>>'{data,email_address}' AS email_address,
	l."data"::json#>>'{data,template_id}' AS template_id,
	l."data"::json#>>'{data,subject}' AS subject,
	l."data"::json#>>'{data,href}' AS href,
	l."data"::json#>>'{data,link_id}' AS link_id,
	l.event_id,
	to_timestamp((l."data"::json->>'timestamp')::bigint) AS timestamp,
	l."data"::json->>'event_type' AS event_type
FROM cio_mysql.event_log l
WHERE l."data"::json->>'event_type' IN 
	('email_opened', 
	'email_converted',
	'email_unsubscribed',
	'email_clicked');
	
