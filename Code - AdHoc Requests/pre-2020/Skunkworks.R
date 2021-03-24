#https://trello.com/c/euZoIqIS/1410-skunkworks-user-retention

#COUNT MAMs for each newsletter by month (look at past 3 months)*/
skunkworks_mam <- sql ("SELECT roundups.*, ask_Freddie.COUNT_askfreddie, Spot_signs.COUNT_spotsigns, weeklydo.COUNT_weeklydo
                        FROM ((
                         SELECT 
                         extract(month FROM mel.timestamp) AS month_active,
                         COUNT(DISTINCT(mel.northstar_id)) AS COUNT_roundup
                         FROM public.member_event_log mel
                         INNER JOIN public.email_event cio
                         ON mel.northstar_id=cio.customer_id
                         WHERE (cio.template_id  IN (2667,2668,2669) AND cio.event_type = 'email_opened' AND mel.timestamp >= '2018-09-04' AND mel.timestamp < '2018-12-01')
                         GROUP BY 1) roundups
                       INNER JOIN
                         (SELECT extract(month FROM mel.timestamp) AS month_active,
                         COUNT(DISTINCT(mel.northstar_id)) AS COUNT_askFreddie
                         FROM public.member_event_log mel
                         INNER JOIN public.email_event cio
                         ON mel.northstar_id=cio.customer_id
                         WHERE (cio.template_id  IN (2670,2673,2675) AND cio.event_type = 'email_opened' AND mel.timestamp >= '2018-09-04' AND mel.timestamp < '2018-12-01')
                         GROUP BY  1) as ask_Freddie 
                         ON ask_Freddie.month_active = roundups.month_active
                       INNER JOIN
                         (SELECT extract(month FROM mel.timestamp) AS month_active,
                         COUNT(DISTINCT(mel.northstar_id)) AS COUNT_spotsigns
                         FROM public.member_event_log mel
                         INNER JOIN public.email_event cio
                         ON mel.northstar_id=cio.customer_id
                         WHERE (cio.template_id  IN (2676,2679) AND cio.event_type = 'email_opened' AND mel.timestamp >= '2018-09-04' AND mel.timestamp < '2018-12-01')
                         GROUP BY 1) Spot_signs
                         ON Spot_signs.month_active = ask_Freddie.month_active)
                       INNER JOIN
                         (SELECT EXTRACT(MONTH FROM mel.timestamp) AS month_active,
                         COUNT(DISTINCT(mel.northstar_id)) AS COUNT_weeklydo
                         FROM public.member_event_log mel
                         INNER JOIN public.email_event cio
                         ON mel.northstar_id=cio.customer_id
                         WHERE (cio.template_id IN (2648) AND cio.template_id NOT IN (2667,2668,2669,2670,2673,2675,2676,2679)
                         AND cio.event_type = 'email_opened' 
                         AND mel.timestamp >= '2018-09-04' AND mel.timestamp < '2018-12-01') 
                         GROUP BY 1) AS weeklydo
                          ON weeklydo.month_active = Spot_signs.month_active")

skunkworks_mam <- runQuery(skunkworks_mam)

#Pull total opens across the newsletters so we have a denominator for each newsletter
skunkworks_opens <- sql("SELECT COUNT(DISTINCT(case when cio.template_id IN (2667,2668,2669) and cio.event_type = 'email_opened' then cio.customer_id else null end)) as COUNT_roundup_opened,
                         COUNT(DISTINCT(case when cio.template_id IN (2670,2673,2675) and cio.event_type = 'email_opened' then cio.customer_id else null end)) as COUNT_askfreddie_opened,
                         COUNT(DISTINCT(case when cio.template_id IN (2676,2679) and cio.event_type = 'email_opened' then cio.customer_id else null end)) as COUNT_spotsigns_opened,
                         COUNT(DISTINCT(CASE WHEN cio.template_id IN (2648) AND cio.template_id NOT IN (2667,2668,2669,2670,2673,2675,2676,2679) 
                         AND cio.event_type = 'email_opened' THEN cio.customer_id ELSE NULL END)) as COUNT_weeklydo_opened
                         FROM public.email_event cio")

skunkworks_opens <- runQuery(skunkworks_opens)

#Calculate percentage of MAMS for each group that opened each newsletter
skunkworks <- skunkworks_mam%>%
  mutate(roundup = case_when(count_roundup>0 ~ round(count_roundup/skunkworks_opens$count_roundup_opened,4)),
         askfreddie = case_when(count_askfreddie>0 ~ round(count_askfreddie/skunkworks_opens$count_askfreddie_opened,4)),
         spotsigns = case_when(count_spotsigns>0 ~ round(count_spotsigns/skunkworks_opens$count_spotsigns_opened,4)),
         weeklydo = case_when(count_weeklydo>0 ~ round(count_weeklydo/skunkworks_opens$count_weeklydo_opened,4)),
         month_active =
           factor(
             case_when(month_active=='9' ~ 'September',
                     month_active=='10' ~ 'October',
                     month_active=='11' ~ 'November'),
             levels = c ('September', 'October', 'November')
             )
          )


#melt from wide to long
skunkworks_pct <- skunkworks%>%
  select(month_active,roundup,askfreddie,spotsigns,weeklydo)%>%
  melt(id.vars = "month_active", measure.vars = c("roundup", "askfreddie", "spotsigns", "weeklydo"))

#chart
ggplot(skunkworks_pct, aes(x=variable, y=value, group = month_active, fill=month_active)) + geom_bar(stat="identity", position = "dodge", colour="grey") +  geom_text(aes(label=scales::percent(value)), vjust=2, position = position_dodge(width=0.9),  size=2) + labs(x = 'Newsletter', y = 'Percentage of MAMS') + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + theme(axis.text.x = element_text(angle = 20, hjust = 1))

