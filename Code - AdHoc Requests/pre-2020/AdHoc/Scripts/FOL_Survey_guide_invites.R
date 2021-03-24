#Grab members who were sent FOL Survey invites on 11/4 and 12/5
fol_surveyed <- glue_sql("SELECT 
	                        sms.user_id  AS id
                          FROM public.gambit_messages_outbound sms
                          WHERE (sms.text LIKE '%t''s Freddie! Thanks for learning how to keep your friends safe on the road. Take our quick survey for a chance to win a gift card!%' 
                          OR sms.text LIKE '%Thanks for learning how to keep you and your friends safe on the road. Take our quick survey for a chance to win a gift card of your choice!%')
                          GROUP BY 1")

fol_surveyed <- runQuery(fol_surveyed)

#Grab members who have viewed the FOL guide/conversation starter pages for at least 1 min and have signed up for FOL
#exclude those who were already sent survey invites on 11/4 and 12/5 (from fol_surveyed above)
fol_guide <- glue_sql("SELECT pe.northstar_id  AS id
                       FROM public.phoenix_events pe
                       INNER JOIN public.phoenix_sessions ps ON ps.session_id = pe.session_id 
                       INNER JOIN public.campaign_activity ca ON ca.northstar_id = pe.northstar_id
                       WHERE (pe.href LIKE 'https://www.dosomething.org/us/articles/friends-on-lock-resource-guide%' 
                       OR pe.href LIKE '%https://www.dosomething.org/us/articles/friends-on-lock-conversation-starters%') 
                       AND ((pe.northstar_id IS NOT NULL)) 
                       AND ((ps.end_datetime - ps.landing_datetime) >= INTERVAL '60 SECONDS') 
                       AND ca.campaign_run_id = '8227'
                       AND ca.northstar_id NOT IN ({nsids*})
                       GROUP BY 1",
                        nsids = fol_surveyed$id,
                        .con = pg)

fol_guide <-runQuery(fol_guide)

write.csv(fol_guide, file = "FOL Survey invites who viewed guides Dec 7.csv")

#Check how many are email users
# guide_email <- glue_sql("SELECT count(u.northstar_id) AS count
#                          FROM public.users u
#                          WHERE (u.northstar_id IN ({nsids*})
#                          AND u.email IS NOT NULL)",
#                           nsids = fol_guide$id,
#                           .con = pg)
# 
# guide_email <- runQuery(guide_email)

