SELECT DISTINCT
	users.northstar_id 
FROM public.campaign_activity AS campaign_activity
INNER JOIN public.users  AS users ON campaign_activity.northstar_id = users.northstar_id
WHERE (campaign_activity.signup_created_at) >= '2017-06-13'
AND users.cio_status = 'customer_subscribed'
AND (users.email is not null 
 	or users.email NOT LIKE '%dosomething.org%' 
	or users.email NOT LIKE '%gmaiil.com%' 
	or users.email NOT LIKE '%postmaster@%'
	or users.email NOT LIKE '%abuse@%'
	or users.email NOT LIKE '%info@%'
	or users.email NOT LIKE '%sales@%'
	or users.email NOT like '%ayhoo.com%'
	or users.email NOT LIKE '%gmial.com%'
	or users.email not like '%yahooo.com%'
	or users.email NOT LIKE '%@dosomething.invalid%'
	or users.email NOT LIKE '%@mobile.import%')
AND (users.country NOT IN ('AT', 'BE', 'BG', 'HR', 'CY', 'CZ', 'DK', 'EU', 'EE', 'FI', 'FR', 'DE', 'GR', 'HU', 'IS', 'IE', 'IT', 'LV', 'LI', 'LT', 'LU', 'MT', 'NL', 'NO', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'CH', 'GB', 'UK'))