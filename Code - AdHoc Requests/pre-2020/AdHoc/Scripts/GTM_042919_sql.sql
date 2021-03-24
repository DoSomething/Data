-- trello request: https://trello.com/c/W0EUPHdT/1482-data-request-grab-the-mic

-- total number of users signing up: 379654; total users reporting back: 128745
SELECT
	count(DISTINCT f.northstar_id) AS num_users_signup,
	count(DISTINCT CASE WHEN f.post_id IS NOT NULL THEN f.northstar_id END) AS num_users_rb
FROM (
	SELECT s.northstar_id, r.post_id
	FROM signups s
	LEFT JOIN reportbacks r
	ON s.id = r.signup_id
	WHERE s.campaign_id = '8017' AND s.created_at < '2019-01-01'
) f


-- number of users whose first campaign is 8017: 161982
SELECT count(DISTINCT northstar_id)
FROM (
	SELECT northstar_id, first_value(campaign_id) OVER (PARTITION BY northstar_id ORDER BY created_at) AS first_campaign
	FROM signups
	WHERE created_at < '2019-01-01'
) f
WHERE f.first_campaign = '8017'


-- number of above users reporting back: 73735
SELECT count(DISTINCT northstar_id)
FROM reportbacks
WHERE northstar_id IN (
	SELECT DISTINCT northstar_id
	FROM (
		SELECT
			northstar_id,
			first_value(campaign_id) OVER (PARTITION BY northstar_id ORDER BY created_at) AS first_campaign
		FROM signups
		WHERE created_at < '2019-01-01'
	) f
	WHERE f.first_campaign = '8017'
)
AND campaign_id = '8017'


-- what percent of all 2018+ reportbacks submitted by GTM members were civic? 116968 vs. 61236
-- for comparison, members who did not signup for GTM: 14571 vs. 73773 88344
SELECT civic_action, count(DISTINCT post_id)
FROM reportbacks
WHERE northstar_id NOT IN (  -- change this condition to NOT IN to find non-GTM members
	SELECT DISTINCT northstar_id
	FROM signups
	WHERE campaign_id = '8017'
)
AND post_created_at >= '2018-01-01'
AND post_created_at < '2019-01-01'
GROUP BY civic_action

