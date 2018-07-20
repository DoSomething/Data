# Follow up on  data request here https://trello.com/c/nGpucST5/1317-6-28-sms-broadcast-data-pull
#Pull # members who clicked on last 3 urls highlighted in yellow in google sheet

source('config/pgConnect.R')

channel <- pgConnect()

# Pull people clicking on Supreme Ct link (https://www.dosomething.org/us/supreme-court-travel-ban-upheld?user_id={{user.id}}&broadcastid=4dYjJLZ5KUE2SOE2OgSIs4)
clicks <- ("SELECT
b.northstar_id,
count(b.click_id) AS total_clicks,
b.target_url,
b.click_time
FROM public.bertly_clicks b
WHERE b.click_time >= '2018-06-28' and b.target_url ilike '%broadcastid=4dYjJLZ5KUE2SOE2OgSIs4%'
GROUP BY b.northstar_id, b.target_url, b.click_time")

clicks_supreme_ct<- runQuery(clicks)

#remove duplicate clicks, count only distinct nsids
clicks_supreme_ct_nodup<- clicks_supreme_ct%>%
  filter(!duplicated(northstar_id))

# Pull people clicking on separation article (no broadcast id was tagged so only look at urls without broadcast source or id)
# https://www.dosomething.org/us/family-separations-us-border?user_id={{user.id}}
clicks_separations <- ("SELECT
                       b.northstar_id,
                       count(b.click_id) AS total_clicks,
                       b.target_url,
                       b.click_time
                       FROM public.bertly_clicks b
                       WHERE b.click_time >= '2018-06-28' and b.target_url ilike '%https://www.dosomething.org/us/family-separations-us-border?user_id%' and b.target_url not like '%broadcastsource%' and b.target_url not like '%broadcastid%'
                       GROUP BY b.northstar_id, b.target_url, b.click_time")

clicks_separations<- runQuery(clicks_separations)

#remove duplicate clicks, only count total number of people who clicked
clicks_separations_nodup<- clicks_separations%>%
  filter(!duplicated(northstar_id))


# Pull number of people clicking on Defend Dreamers (https://www.dosomething.org/us/campaigns/defend-dreamers/blocks/6DUwj4oO88e4ga4cQmgEiA?user_id={{user.id}})
clicks_defend_dreamers <- read.csv('~/Desktop/defend dreamers clicks.csv')

clicks_defend_dreamers_nodup<- clicks_defend_dreamers%>%
  filter(!duplicated(northstar_id))
