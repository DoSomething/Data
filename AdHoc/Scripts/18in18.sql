SELECT u.northstar_id, u.birthdate 
FROM quasar.users u
WHERE (u.customer_io_subscription_status = 'subscribed' OR u.moco_current_status = 'active') 
AND u.birthdate <= DATE('2000-11-07') 
AND u.birthdate >= DATE('2000-01-01')
