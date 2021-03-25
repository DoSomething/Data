SELECT *
FROM quasar.users u 
WHERE u.birthdate > DATE('0000-00-00') 
AND u.birthdate <= DATE('1977-00-00')
AND (u.customer_io_subscription_status = 'subscribed' 
	OR u.moco_current_status = 'active')