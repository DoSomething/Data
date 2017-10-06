SELECT DISTINCT 
	temp_accepted.status, 
	temp_accepted.signup_id AS 'accepted', 
	temp_rej.signup_id AS 'rejected', 
	temp_rej.status
FROM(
	(SELECT 
		(CASE WHEN ca.status = 'accepted' 
			THEN ca.signup_id ELSE NULL END) AS 'status', 
		ca.signup_id as 'signup_id' 
		FROM quasar.campaign_activity ca 
		WHERE ca.status IS NOT NULL 
		AND ca.post_id > 0
		) temp_accepted 
	LEFT JOIN
		(SELECT 
			(CASE WHEN rej.status = 'rejected' 
				THEN rej.signup_id ELSE NULL END) AS 'status', 
			rej.signup_id as 'signup_id'
		FROM quasar.campaign_activity rej
		WHERE rej.status IS NOT NULL 
		AND rej.post_id > 0
		) temp_rej
	ON temp_rej.signup_id = temp_accepted.signup_id)
WHERE temp_accepted.status IS NOT NULL 
AND temp_rej.status IS NOT NULL
LIMIT 10