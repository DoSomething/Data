#https://trello.com/c/ABBD6RD5/1384-data-request-sms-inbound-messages

#Pull emails
TNT <- ("SELECT 
	      gambit.created_at AS time,
        gambit.user_id  AS northstar_id,
        gambit.text  AS email
        FROM public.gambit_messages_inbound AS gambit
        WHERE gambit.topic  IN ('tmi_level1') 
        AND gambit.created_at >= now() - interval '90 DAYS'
        GROUP BY 1,2,3
        ORDER BY 2 DESC")

TNT_emails <- runQuery(TNT)

#Pull companies
TNT_co <- ("SELECT 
	      gambit.created_at AS time,
        gambit.user_id  AS northstar_id,
        gambit.text  AS company
        FROM public.gambit_messages_inbound AS gambit
        WHERE gambit.topic  IN ('tmi_completed') 
        AND gambit.created_at >= now() - interval '90 DAYS'
        GROUP BY 1,2,3
        ORDER BY 2 DESC")

TNT_companies <- runQuery(TNT_co)

#Merge two files to create table with email and company messaged per nsid
TNT_all <- TNT_emails %>%
  left_join(TNT_companies, by = 'northstar_id')%>%
  select(-time.y)

write.csv(TNT_all, file = "TNT SMS Subscribes in last 3 months.csv")
