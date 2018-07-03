-- Voter reg data wrangling 
	-- dataset would need to be grouped by unique northstar_id with all relevant variables that are spread in R 
	-- pull each necessary dataset and manipulate in R 

-- USERS data that includes the following variables AND is a member, from the US & over 18 

SELECT 
	u.northstar_id,
	u.created_at,
	u.last_logged_in,
	u.last_accessed,
	u.SOURCE,
	u.email,
	u.mobile,
	u.birthdate,
	u.first_name,
	u.last_name,
	u.city,
	u.state,
	u.zipcode,
	u.cio_status,
	u.sms_status
FROM users u
WHERE u.subscribed_member = 'true'
	AND u.country = 'US'
	AND u.birthdate <= current_date - INTERVAL '18 years'


-- PHOENIX_EVENTS where northstar_id is not null

SELECT 
	p.northstar_id,
	p.event_datetime,
	p.event_name,
	p.event_source,
	p.campaign_id,
	p.campaign_name,
	p.page_utm_medium,
	p.page_utm_campaign,
	p.parent_source,
	p.browser_size
FROM phoenix_events p
WHERE p.northstar_id IS NOT NULL 


-- CAMPAIGN_ACTIVITY 

SELECT 
	c.northstar_id,
	c.campaign_id,
	c.campaign_run_id,
	c.post_type,
	c.post_action,
	c.post_class,
	c.reportback_volume,
	c.post_status,
	c.why_participated,
	c.quantity,
	c.signup_source,
	c.post_source,
	c.signup_created_at,
	c.reported_back,
	c.url,
	c.post_attribution_date
FROM campaign_activity c
WHERE c.northstar_id IS NOT NULL 


-- EMAIL_EVENT 

SELECT 
	e.customer_id,
	e.email_address,
	e.subject,
	e."timestamp",
	e.event_type
FROM email_event e 
WHERE customer_id IS NOT NULL 
	AND email_address IS NOT NULL 

SELECT count(*)
FROM email_event
	

-- MEMBER_EVENT_LOG 

SELECT 
	m.northstar_id,
	m."timestamp",
	m.action_type,
	m.SOURCE
	-- m.action_serial_id? 
FROM member_event_log m
WHERE m.northstar_id IS NOT NULL 

SELECT *
FROM campaign_activity
LIMIT 30


