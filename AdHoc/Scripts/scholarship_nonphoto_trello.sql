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


-- 3. create table of select 30 winners 

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
        sum(ca.quantity + COALESCE(shares.reportback_volume, 0)) AS total_shares
    FROM campaign_activity ca
    LEFT JOIN users u -- left joining users 
        on u.northstar_id = ca.northstar_id
    LEFT JOIN  -- left joining shares data 
        (SELECT  
            DISTINCT e.northstar_id,
            1 AS reportback_volume
        FROM phoenix_events e
        WHERE e.event_name = 'share action completed'
            AND e.campaign_name = 'grab-mic') shares ON shares.northstar_id = ca.northstar_id -- change 'grab-mic' for diff campaign
    WHERE ca.campaign_run_id = 8022 -- change '8022' for diff campaign 
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
    HAVING sum(ca.quantity + COALESCE(shares.reportback_volume, 0)) > 0) AS winners 
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
        sum(ca.quantity + COALESCE(shares.reportback_volume, 0)) AS total_shares
    FROM campaign_activity ca
    LEFT JOIN users u -- left joining users 
        on u.northstar_id = ca.northstar_id
    LEFT JOIN  -- left joining shares data 
        (SELECT  
            DISTINCT e.northstar_id,
            1 AS reportback_volume
        FROM phoenix_events e
        WHERE e.event_name = 'share action completed'
            AND e.campaign_name = 'grab-mic') shares ON shares.northstar_id = ca.northstar_id -- change 'grab-mic' for diff campaign
    WHERE ca.campaign_run_id = 8022 -- change '8022' for diff campaign 
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
    HAVING sum(ca.quantity + COALESCE(shares.reportback_volume, 0)) > 0) AS winners 
ORDER BY winners.total_shares / random() DESC 
-- multiplying total shares number by random number, ranging from 0-1.
-- This is pretty straightforward, and the 30 that come up are all those who have done 6+ shares.
-- I would recommend this approach if we want to pick a winner who is very active and has taken many actions.
LIMIT 30;

-- another approach is dividing total shares by a random number. 
-- This gives 1 or 2 highly active members and then the rest are those who have done 1-4 shares.


winners.total_shares * random()


SELECT quantity 
FROM campaign_activity
WHERE quantity IS NOT null









	
	
	
	
	
	
	
	
