SELECT setseed(.8);
SELECT
    winners.* 
FROM -- creating a winners dataset 
    (SELECT s.northstar_id, -- selecting from campaign activity 
        s.id AS signup_id,
        s.created_at AS signup_created_at,
        u.first_name,
        u.email,
        u.mobile,
        sum(r.reportback_volume) AS reportbacks
    FROM signups s
    INNER JOIN reportbacks r ON s.id = r.signup_id
    LEFT JOIN users u -- left joining users 
        on u.northstar_id = s.northstar_id
    WHERE s.campaign_id = '8017' -- use campaign_id in place of **** 
    	AND u.first_name IS NOT NULL
        AND u.email IS NOT NULL
        AND r.post_id IS NOT NULL 
        AND r.post_status = 'accepted'
        AND r.post_type <> 'voter-reg'
        AND s.created_at >= '2018-12-01' AND s.created_at < '2019-01-01'
    GROUP BY s.northstar_id,
            s.id,
            s.created_at,
            u.first_name,
            u.email,
            u.mobile
    HAVING sum(r.reportback_volume) > 0
    ) AS winners 
ORDER BY -log(random())
LIMIT 30;




-- WEIGHTED

SELECT setseed(.7);
SELECT
    winners.* 
FROM -- creating a winners dataset 
    (SELECT s.northstar_id, -- selecting from campaign activity 
        s.id AS signup_id,
        s.created_at AS signup_created_at,
        u.first_name,
        u.email,
        u.mobile,
        sum(r.reportback_volume) AS reportbacks
    FROM signups s
    INNER JOIN reportbacks r ON s.id = r.signup_id
    LEFT JOIN users u -- left joining users 
        on u.northstar_id = s.northstar_id
    WHERE s.campaign_id = '8017' -- use campaign_id in place of **** 
    	AND u.first_name IS NOT NULL
        AND u.email IS NOT NULL
        AND r.post_id IS NOT NULL 
        AND r.post_status = 'accepted'
        AND r.post_type <> 'voter-reg'
        AND s.created_at >= '2018-11-01' AND s.created_at < '2018-12-01'
    GROUP BY s.northstar_id,
            s.id,
            s.created_at,
            u.first_name,
            u.email,
            u.mobile
    HAVING sum(r.reportback_volume) > 0) AS winners 
ORDER BY log(winners.reportbacks) / random() DESC -- This gives 4-5 highly active members and then the rest are those who have done 1-4 shares.
LIMIT 30;

