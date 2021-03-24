SELECT DISTINCT
	ca.northstar_id,
	ca.campaign_run_id,
	count(DISTINCT ca.quantity) AS total_cards,
	max(CASE WHEN ca.campaign_run_id IN (6172) THEN 1 ELSE 0 END) AS quitters_win, 
	max(CASE WHEN ca.campaign_run_id IN (7439) THEN 1 ELSE 0 END) AS love_letters
FROM quasar.campaign_activity ca
GROUP BY ca.northstar_id

/*Number of members who participated in Love Letters and returned a card*/
SELECT count(distinct ca.northstar_id) as love_letter_participants
FROM quasar.campaign_activity ca
WHERE ca.campaign_run_id IN (7439) and quantity>0

/*Total number of cards for Love Letters*/
SELECT 
	count(ca.quantity) AS total_cards
FROM quasar.campaign_activity ca
WHERE ca.campaign_run_id IN (7439)

/*Number of members who didn't participate in Love Letters*/
SELECT count(distinct ca.northstar_id) as non_love_letter_participants
FROM quasar.campaign_activity ca
WHERE ca.campaign_run_id != (7439) 


/*Number of members who participated in Quitters Always Win and returned a card*/
SELECT count(distinct ca.northstar_id) as quitters_participants
FROM quasar.campaign_activity ca
WHERE ca.campaign_run_id IN (6172) and quantity>0

/*Number of members who didn't participate in Quitters Always Win*/
SELECT count(distinct ca.northstar_id) as nonquitters_participants
FROM quasar.campaign_activity ca
WHERE ca.campaign_run_id != (6172) 
	