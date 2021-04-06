SELECT 
	count(distinct a.northstar_id),
	a.total_signups
FROM
	(SELECT
		ca.northstar_id,
		count(distinct ca.signup_id) AS 'total_signups'
	FROM
		campaign_activity ca
	GROUP BY ca.northstar_id)
	AS a
GROUP BY a.total_signups
;

--vFINAL

SELECT
	u.northstar_id AS "northstar_id",
	IFNULL(count(distinct ca.signup_id), 0) AS 'total_signups',
	u.source
FROM
  	quasar.users u
LEFT JOIN
	quasar.campaign_activity ca
ON
	u.northstar_id = ca.northstar_id
WHERE
	(u.customer_io_subscription_status = 'subscribed' OR u.sms_status = 'active')
GROUP BY
	u.northstar_id
