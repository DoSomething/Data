#https://trello.com/c/LgSDsmM7/1288-data-request-grab-the-mic-member-engagement

source('config/init.R')
source('config/pgConnect.R')
library(glue)
library(dplyr)
library(RMySQL)
library(ggthemes)
library(lubridate)
pg <- pgConnect()


## 3 GTM members who have gone on to complete an action on another campaign 

q3 <- "select northstar_id, 
		campaign_run_id,
signup_created_at,
post_attribution_date
from campaign_activity
where campaign_run_id = 8022 and post_created_at is not null"

qres3_gtm <- runQuery(q3, "pg")

q3b <- "select northstar_id,
		campaign_run_id,
signup_created_at,
post_created_at
from campaign_activity
where campaign_run_id <> 8022 and post_created_at is not null"

qres3_not_gtm <- runQuery(q3b, "pg") #for some reason removed time stamp (hour, minute, second) so pulled csv as a workaround 

not_gtm <- read.csv("/Users/dosomething/not_gtm.csv", stringsAsFactors = FALSE)
gtm <- read.csv("/Users/dosomething/gtm.csv", stringsAsFactors = FALSE)

gtm_signup_action <- inner_join(gtm, not_gtm, by = "northstar_id") 

#post_created_at.y = action date/time stamp 
#signup_created_at.x = sign-up date/time stamp
gtm_signup_action$post_created_at.y <- as.POSIXct(gtm_signup_action$post_created_at.y, 
                                                  format = "%Y-%m-%d %H:%M:%S")
gtm_signup_action$signup_created_at.x <- as.POSIXct(gtm_signup_action$signup_created_at.x, 
                                                  format = "%Y-%m-%d %H:%M:%S")

gtm_followup_action <- unique(gtm_signup_action %>% 
                 filter(post_created_at.y > signup_created_at.x) %>% 
                 select(northstar_id))

write.csv(gtm_followup_action, 
          file = "/Users/dosomething/Documents/3_GTM_action_on_anothercampaign.csv",
          row.names=FALSE)


## 8. Avg time GTM members spent on site 

## session information (landing & end time stamp)
q8 <- "SELECT
    e.northstar_id,
e.session_id,
max(to_timestamp(e.ts/1000)) AS ending_ts,
min(to_timestamp(s.landing_ts/1000)) AS landing_ts
FROM phoenix_events e
LEFT JOIN phoenix_sessions s ON s.session_id = e.session_id
WHERE e.northstar_id IS NOT NULL
AND e.northstar_id <> ''
GROUP BY e.northstar_id, e.session_id"

qres8 <- runQuery(q8, "pg")

q8b <- "select 
	distinct c.northstar_id
from campaign_activity c
where c.campaign_run_id=8022"

qres8b <- runQuery(q8b, "pg")

session_data <- inner_join(qres8, qres8b, by = "northstar_id")

#average time spent = 4.43 minutes 
mean(difftime(session_data$ending_ts, session_data$landing_ts, units = "mins"))

#a quick table with buckets and percentage 
duration_buckets <- 
  session_data %>% 
  mutate(duration = difftime(ending_ts, landing_ts, units = "mins")) %>%
  summarise(zero = sum(duration == 0) / length(duration),
            less_thanone = sum(duration > 0 & duration < 1) / length(duration),
            betwn_one_five = sum(duration >= 1 & duration < 5) / length(duration),
            betwn_five_ten = sum(duration >= 5 & duration < 10) / length(duration),
            betwn_ten_twenty = sum(duration >= 10 & duration < 20) / length(duration),
            more_thantwenty = sum(duration >= 20) / length(duration))

t(duration_buckets) 

# ggplot 
time_spent <- 
  session_data %>%
  mutate(min_spent = difftime(ending_ts, landing_ts, units = "mins"))

ggplot(time_spent) +
  geom_density(aes(x = min_spent), fill = "purple") +
  xlim(0, 50) +
  theme_tufte() +
  labs(title = "Time that GTM members spend on website",
       x = "In Minutes", 
       y = "")
ggsave("duration_plot.png", path = "/Users/dosomething/Documents")


## 10. How many members have taken 2+ actions? 

q10 <- "select 
	c.northstar_id,
c.post_id,
c.post_type,
c.post_action
from public.campaign_activity c
where c.campaign_run_id=8022"

qres10 <- runQuery(q10, "pg")

question10 <- 
  qres10 %>%
  filter(!is.na(post_id)) %>%
  select(northstar_id, post_id) %>%
  group_by(northstar_id) %>% 
  tally %>%
  filter(n > 1) %>% 
  count()

## 11. What actions are members taking the most often?

q11_byaction <-
  qres10 %>%
  filter(!is.na(post_action)) %>%
  group_by(post_action) %>% 
  tally %>%
  arrange(desc(n))

q11_bytype <- 
  qres10 %>%
  filter(!is.na(post_type)) %>%
  group_by(post_type) %>%
  tally %>%
  arrange(desc(n))

## 12. Breakdown of actions by month 

q12 <- "select 
	c.northstar_id,
	c.post_id,
	c.post_type,
	c.post_action,
	c.post_created_at
from public.campaign_activity c
where c.campaign_run_id=8022 
	AND post_created_at IS NOT NULL "

qres12 <- runQuery(q12, "pg")

qres_2018$month <- month(qres12$post_created_at)

qres_2018_actiontype <- 
  qres12 %>% 
  mutate(month = month(qres12$post_created_at)) %>%
  filter(year(qres12$post_created_at) == 2018) %>% 
  group_by(month, post_action) %>%
  tally 

qres_2018_posttype <- 
  qres12 %>% 
  mutate(month = month(qres12$post_created_at)) %>%
  filter(year(qres12$post_created_at) == 2018) %>% 
  group_by(month, post_type) %>%
  tally 


# 16. How many members have gone on to complete an action on another campaign (NOT GTM)? 

nrow(gtm_followup_action) # from question 3










  








