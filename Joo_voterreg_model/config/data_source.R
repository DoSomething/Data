# Users dataset 

users_query <- 
  "SELECT u.northstar_id,
u.created_at,
u.last_logged_in,
u.last_accessed,
u.SOURCE,
u.birthdate,
u.first_name,
u.state,
u.zipcode,
u.cio_status,
u.sms_status,
u.voter_registration_status
FROM users u
WHERE u.subscribed_member = 'true'
AND u.country = 'US'
AND u.birthdate <= current_date - INTERVAL '18 years'" # removing underage members in users file 

# phoenix_events dataset 

phoenix_query <- "SELECT 
northstar_id,
event_datetime,
event_name,
event_source,
campaign_id,
page_utm_medium,
parent_source,
browser_size
FROM phoenix_events
WHERE northstar_id IS NOT NULL 
AND event_name IN ('view', 'visit', 'open modal', 'share action completed', 'clicked voter registration action', 'facebook share posted', 'photo-submission_action', 'clicked register-submit', 'Successful Reportback', 'text-submission-action', 'referral-submission-action')"

# member_event_log dataset 

mel_query <- "SELECT 
m.northstar_id,
m.timestamp,
m.action_type,
m.SOURCE
FROM member_event_log m
WHERE m.northstar_id IS NOT NULL"

# campaign_activity dataset 

ca_query <- "SELECT c.northstar_id,
c.campaign_id,
c.campaign_run_id,
c.post_type,
c.reportback_volume,
c.post_status,
c.quantity,
c.signup_source,
c.post_source,
c.signup_created_at,
c.reported_back,
c.post_attribution_date
FROM campaign_activity c
WHERE c.northstar_id IS NOT NULL"

# campaign_info

ca_info_query <- "SELECT 
campaign_run_id,
campaign_type,
campaign_verb,
campaign_cause_type
FROM campaign_info
WHERE campaign_run_id IS NOT NULL"

# email data

email_query <- "SELECT customer_id AS northstar_id,
email_address,
template_id, 
timestamp,
event_type
FROM email_event 
WHERE customer_id IS NOT NULL"

# SMS

SMS_mes <- "SELECT 
northstar_id,
click_time AS SMS_time,
'SMS message' AS action_type
FROM bertly_clicks
WHERE northstar_id IS NOT NULL
UNION ALL 
SELECT 
id AS northstar_id,
last_messaged_at AS SMS_time,
'SMS link click' AS action_type
FROM northstar.users 
WHERE id IS NOT NULL 
AND last_messaged_at IS NOT NULL"

# Turbovote file 

turbo_query <- "SELECT 
nsid AS northstar_id,
SOURCE,
campaign_id,
ds_vr_status,
ds_registration_date,
file
FROM turbovote_file
WHERE nsid IS NOT null"

# voter_reg_predictions file 

voter_reg_table_query <- "SELECT 
northstar_id,
voter_reg_status
FROM voter_reg_predictions
"



