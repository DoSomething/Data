-- https://trello.com/c/XYCNkQUz/1299-data-request-new-scholarship-query-to-include-non-photo-rbs


-- social shares only on puck data 
	-- join in puck data 
	-- give people an entry for every report back 
	
-- phoenix_events event_name = 'share action completed'
	-- has all candidates who have shared on social 
	-- take the unweighted, throw away 
	

-- Broken down into temp tables -- 

-- 1. temporary "shares" table that includes distinct id's who have shared on grab the mic (change 'grab-mic' for other campaigns)

create temporary table shares as 
	select 
		distinct e.northstar_id,
		1 as reportback_volume
	from phoenix_events e 
	where e.event_name = 'share action completed'
		and e.campaign_name = 'grab-mic'


-- 2. temporary "winners" table that joins campaign_activity, users, and temporary shares data	

create temporary table winners as 
	SELECT 
		ca.northstar_id,
		ca.signup_id,
		ca.signup_created_at,
		u.first_name,
		u.email,
		u.mobile,
		SUM(ca.reportback_volume + COALESCE(shares.reportback_volume,0)) AS total_quantity 
	-- so all shares.reportback_volume is 1... should coalesce be around ca.reportback_volume (which has null values)?
	-- BUT does not work...
	FROM campaign_activity ca 
	LEFT JOIN users u 
		ON u.northstar_id = ca.northstar_id 
	LEFT JOIN shares
		on shares.northstar_id = ca.northstar_id 
	WHERE ca.campaign_run_id = 8022 -- change this to select a scholarship winner from another campaign 
	GROUP BY ca.northstar_id, 
		ca.signup_id,
		ca.signup_created_at,
		u.first_name,
		u.email,
		u.mobile
	HAVING SUM(ca.reportback_volume + COALESCE(shares.reportback_volume,0)) > 0


-- 3. create table of select 30 winners.

select 
	winners.*
from winners 
ORDER BY -log(random()) 
LIMIT 30


-- All in one query --

-- UNWEIGHTED

SELECT setseed(.8);
SELECT
    winners.* 
FROM -- creating a winners dataset 
    (SELECT ca.northstar_id, -- selecting from campaign activity 
        ca.signup_id,
        ca.signup_created_at,
        u.first_name,
        u.email,
        u.mobile,
        sum(ca.reportback_volume + COALESCE(shares.reportback_volume, 0)) AS total_shares
    FROM campaign_activity ca
    LEFT JOIN users u -- left joining users 
        on u.northstar_id = ca.northstar_id
    LEFT JOIN  -- left joining shares data 
        (SELECT  
            DISTINCT e.northstar_id,
            1 AS reportback_volume
        FROM phoenix_events e
        WHERE e.event_name = 'share action completed'
            AND e.campaign_name = '****') shares ON shares.northstar_id = ca.northstar_id -- use campaign_name in place of '****'  
    WHERE ca.campaign_run_id = **** -- use campaign_run_id in place of **** 
        AND u.first_name IS NOT NULL
        AND u.email IS NOT NULL
        AND ca.post_id IS NOT NULL 
        AND ca.post_status = 'accepted'
    GROUP BY ca.northstar_id,
            ca.signup_id,
            ca.signup_created_at,
            u.first_name,
            u.email,
            u.mobile
    HAVING sum(ca.reportback_volume + COALESCE(shares.reportback_volume, 0)) > 0) AS winners 
ORDER BY -log(random())
LIMIT 30;




-- WEIGHTED

SELECT setseed(.7);
SELECT
    winners.* 
FROM -- creating a winners dataset 
    (SELECT ca.northstar_id, -- selecting from campaign activity 
        ca.signup_id,
        ca.signup_created_at,
        u.first_name,
        u.email,
        u.mobile,
        sum(ca.reportback_volume + COALESCE(shares.reportback_volume, 0)) AS total_shares
    FROM campaign_activity ca
    LEFT JOIN users u -- left joining users 
        on u.northstar_id = ca.northstar_id
    LEFT JOIN  -- left joining shares data 
        (SELECT  
            DISTINCT e.northstar_id,
            1 AS reportback_volume
        FROM phoenix_events e
        WHERE e.event_name = 'share action completed'
            AND e.campaign_name = '****') shares ON shares.northstar_id = ca.northstar_id -- use campaign_name in place of '****'  
    WHERE ca.campaign_run_id = **** -- use campaign_run_id in place of **** 
        AND u.first_name IS NOT NULL
        AND u.email IS NOT NULL
        AND ca.post_id IS NOT NULL 
        AND ca.post_status = 'accepted'
    GROUP BY ca.northstar_id,
            ca.signup_id,
            ca.signup_created_at,
            u.first_name,
            u.email,
            u.mobile
    HAVING sum(ca.reportback_volume + COALESCE(shares.reportback_volume, 0)) > 0) AS winners 
ORDER BY log(winners.total_shares) / random() DESC -- This gives 4-5 highly active members and then the rest are those who have done 1-4 shares.
LIMIT 30;
















	
	
	
	
	
	
	
	
