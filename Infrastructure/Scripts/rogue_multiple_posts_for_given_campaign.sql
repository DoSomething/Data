#This query is used to identify users that have sent more than one post in for a given campaign run in Rogue.
SELECT count(p.id), p.signup_id
FROM posts p
WHERE p.campaign_id = ####
AND p.created_at > TIMESTAMP('')
GROUP BY p.signup_id
HAVING count(p.id) >1 