#https://trello.com/c/n4qfXkbr/1138-defend-dreamers-week-of-action-analysis

library(ggmap)
library(ggthemes)
library(maps)
library(maptools)
library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)
library(anytime)
library(plyr)
library(stringr)

#Load sources for cleaning phone functions
source("Data/AdHoc/config/DSUtils.R")
source("Data/AdHoc/config/mySQLConfig.R")

##### SQL queries to pull members who participated in Defend Dreamers campaign ####
# data <- runQuery('Desktop/Defend Dreamers action types.sql')

# Pull data from SQl query for those who did Defend Dreamers - Number of signups, age, years a member .csv
signups <-tbl_dt(read.csv('Desktop/Signed up for Defend Dreamers.csv'))%>%
  filter(participated_in_dd==1)%>%
  arrange(-birthdate)%>%
  mutate(age_date=as.Date(birthdate, format='%Y-%m-%d'),
         daysOld = Sys.Date() - age_date,
        age_Today=as.numeric(floor(daysOld/365.25)))

head(signups$birthdate)
head(as.Date(signups$birthdate))

# Members who called reps (data from MoCo that was in Trello request board)
dreamers <- tbl_dt(read.csv('Desktop/Defend Dreamers members.csv'))%>%
  filter(!duplicated(handset_number))%>%
  mutate(handset_number=cleanPhone(handset_number), 
         age_date=as.Date(birthdate),
         daysOld = Sys.Date() - age_date,
         age_Today=as.numeric(floor(daysOld/365.25)))

# Check phone numbers are cleaned up (replace handset_number in code above to 'handset_number_test' to test)
# dreamers %>% select(handset_number, handset_number_test) %>% View()

#Link db data to MoCo data (age, years a member)
dreamers_linked <- dreamers %>%
  left_join(signups, by = 'handset_number')

#Top 5 cities members live in
cities <-dreamers_linked%>%
  group_by(geocoded_city)%>%
  summarise(Count=n())

states <-dreamers_linked%>%
  group_by(geocoded_state)%>%
  summarise(Count=n())

#If age is missing from DB list (signups) then take age from Moco (dreamers)
dreamers_age <-dreamers_linked%>%
  mutate(
 age_rec= ifelse(is.na(age_Today.y), age_Today.x, age_Today.y))

#Age distribution
dreamers_linked_nooutliers <-dreamers_age %>% 
  filter(age_rec>10)

ggplot(dreamers_linked_nooutliers, aes(x=age_rec)) + geom_density()

#Average age of members
dreamers_linked_nooutliers %>% 
  summarise(mean= mean(age_rec), n=n(),quantile(age_rec), range((age_rec)))

#Signup distribution
ggplot(dreamers_linked, aes(x=total_signups)) + geom_density()

#Average # of campaign sign ups
dreamers_linked %>% 
  filter(!is.na(total_signups))%>%
  summarise(mean= mean(total_signups), n=n(),range(total_signups), quantile(total_signups))

#Years a member
dreamers_memberyrs <- dreamers_linked%>%
  filter(!is.na(total_signups))%>%
  mutate(years_a_member=as.numeric(years_a_member))

#Histogram of years a member
hist(dreamers_memberyrs$years_a_member)

#Average of number of years a member
dreamers_memberyrs%>% 
  summarise(mean= mean(years_a_member), n=n(),quantile(years_a_member), range((years_a_member))) 

#Heatmap of where Defend Dreamers members are from
map<-get_map(location='united states', zoom=4, maptype = 'roadmap',
             source='google',color='color')
ggmap(map)

ggmap(map) + 
stat_density2d(data=dreamers_linked, aes(x=longitude, y=latitude, fill=..level.., alpha=..level..), bins=32, geom='polygon') +
scale_fill_gradient(low='green', high='red', guide=FALSE) + 
scale_alpha(range = c(0,1), guide=FALSE) + 
labs(x='Longitude', y='Latitude', title='Heatmap of Defend Dreamers Members')

#Action/cause types .csv from SQl queries
action <- tbl_dt(read.csv('Desktop/Defend Dreamers action types.csv'))
campaigns <-tbl_dt(read_csv('Desktop/Defend Dreamers campaigns.csv'))

#Defend Dreamers - action types
action_dd <- tbl_dt(read.csv('Desktop/Defend Dreamers action types.csv'))%>%
  filter(participated_in_dd==1)

action_dd%>%
  group_by(action_type)%>%
  summarise(
  Count = n(),
  Proportion.DD = n() / nrow(action_dd)) 

sum(hold$Proportion.DD)

action_dd%>%
  group_by(cause_type)%>%
  summarise(
    Count = n(),
    Proportion.DD = n() / nrow(action_dd)) 

#Non-Defend Dreamers action types
action_nondd <- tbl_dt(read.csv('Desktop/Defend Dreamers action types.csv'))%>%
  filter(participated_in_dd==0)

action_nondd%>%
  group_by(action_type)%>%
  summarise(
    Count = n(),
    Proportion.NonDD = n() / nrow(action_nondd)) 

sum(hold$Proportion.NonDD)

action_nondd%>%
  group_by(cause_type)%>%
  summarise(
    Count = n(),
    Proportion.NonDD = n() / nrow(action_nondd)) 

action <- tbl_dt(read.csv('Desktop/Defend Dreamers action types.csv'))

action%>%
group_by(action_type, participated_in_dd)%>%
summarise(
Count = n(),
Proportion.DD = n() / nrow(action[participated_in_dd==1]),
Proportion.NonDD = n() / nrow(action[participated_in_dd==0])
) %>% filter(participated_in_dd==1) 

#campaigns
campaigns <-tbl_dt(read_csv('Desktop/Defend Dreamers campaigns.csv'))

#Non-Defend Dreamers campaign types

campaigns.NonDD <-campaigns%>%
  filter(participated_in_dd==0)

campaigns.NonDD%>%
  group_by(campaign)%>%
  summarise(
    Count = n(),
    Proportion.NonDD = n() / nrow(campaigns.NonDD))

#Defend Dreamers campaign types
campaigns.DD <-dreamers_linked%>%
  left_join(campaigns, by = 'handset_number')

campaigns.DD%>%
  group_by(campaign)%>%
  summarise(
    Count = n(),
    Proportion.DD = n() / nrow(campaigns.DD))


#Week of Actions, clean phone from Moco CSVs (drop leading 1 from phone numbers so it's consistent with dreamers dataset)
# and create identifier for each action day 
day1 <-tbl_dt(read.csv('Desktop/DACA actions/defend_dreamers_week_of_action_day_1_clicks_20170926_125513.csv')
            )%>%
  mutate(action1=1, 
         phone2=cleanPhone(phone))%>%
  select(phone2,action1)%>%
  filter(!duplicated(phone2))
day2fb <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_2_facebook_post_clicks_20170926_125432.csv'))%>%
  mutate(action2fb=1,
         phone2=cleanPhone(phone))%>%
  select(phone2,action2fb)%>%
  filter(!duplicated(phone2))
day2pn <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_2_phoenix_next_clicks_20170926_141649.csv'))%>%
  mutate(action2pn=1,
         phone2=cleanPhone(phone))%>%
  select(phone2,action2pn)%>%
  filter(!duplicated(phone2))
#merge Fb and Phoneix Next day 2 actions, if they clicked on either then they did Day 2 action
day2 <-day2fb%>%
  left_join(day2pn, by = 'phone2')%>%
  mutate(
    action2= ifelse(action2fb==1 | action2pn==1, 1,0))%>%
  select(phone2,action2)%>%
  filter(!duplicated(phone2))
day3 <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_3_dreamers_are_welcome_signs_clicks_20170926_125413.csv'))%>%
  mutate(action3=1, 
         phone2=cleanPhone(phone))%>%
  select(phone2,action3)%>%
  filter(!duplicated(phone2))
day5 <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_5_aclu_email_clicks_20170926_125344.csv'))%>%
  mutate(action5=1,
         phone2=cleanPhone(phone))%>%
  select(phone2,action5)%>%
filter(!duplicated(phone2))
day6 <-tbl_dt(read_csv('Desktop/DACA actions/daca_day_6_facebook_post_clicks_20170926_125129.csv'))%>%
  mutate(action6=1,
         phone2=cleanPhone(phone))%>%
  select(phone2,action6)%>%
  filter(!is.na(phone2))%>%
  filter(!duplicated(phone2))


day1 <-tbl_dt(read.csv('Desktop/DACA actions/defend_dreamers_week_of_action_day_1_clicks_20170926_125513.csv')
)%>%
  mutate(action=1, 
         phone2=cleanPhone(phone))%>%
  select(phone2,action)%>%
day2fb <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_2_facebook_post_clicks_20170926_125432.csv'))%>%
  mutate(action2fb=1,
         phone2=cleanPhone(phone))%>%
  select(phone2,action2fb)%>%
  filter(!duplicated(phone2))%>%
day2pn <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_2_phoenix_next_clicks_20170926_141649.csv'))%>%
  mutate(action2pn=1,
         phone2=cleanPhone(phone))%>%
  select(phone2,action2pn)%>%
  filter(!duplicated(phone2))
#merge Fb and Phoneix Next day 2 actions, if they clicked on either then they did Day 2 action
day2 <-day2fb%>%
  left_join(day2pn, by = 'phone2')%>%
  mutate(
    action= ifelse(action2fb==1 | action2pn==1, 2,0))%>%
  select(phone2,action)%>%
  filter(!duplicated(phone2))
day3 <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_3_dreamers_are_welcome_signs_clicks_20170926_125413.csv'))%>%
  mutate(action=3, 
         phone2=cleanPhone(phone))%>%
  select(phone2,action)%>%
  filter(!duplicated(phone2))
day5 <-tbl_dt(read.csv('Desktop/DACA actions/daca_day_5_aclu_email_clicks_20170926_125344.csv'))%>%
  mutate(action=5,
         phone2=cleanPhone(phone))%>%
  select(phone2,action)%>%
  filter(!duplicated(phone2))
day6 <-tbl_dt(read_csv('Desktop/DACA actions/daca_day_6_facebook_post_clicks_20170926_125129.csv'))%>%
  mutate(action=6,
         phone2=cleanPhone(phone))%>%
  select(phone2,action)%>%
  filter(!is.na(phone2))%>%
  filter(!duplicated(phone2))
#don't need to change phone number format
day7 <-dreamers%>%
  mutate (action=7,
          phone2=handset_number)%>%
  select(phone2,action)

#Join all days
week <-
  day1%>%
  bind_rows(day2%>%
  bind_rows(day3%>%
  bind_rows(day5%>%
  bind_rows(day6%>%
  bind_rows(day7)
))))

#Count actions clicked on in week of actions
counts <-
  week %>%
  group_by(phone2) %>%
  dplyr::summarise(
    Count=n(),
    activeDay1 = max(action==1),
    activeDay2 = max(action==2),
    activeDay3 = max(action==3),
    activeDay5 = max(action==5),
    activeDay6 = max(action==6),
    activeDay7 = max(action==7)
    )

#Look at whether of the people who made calls on Sunday Citizenship Day - 
#how many of those made their first call that week, and did not make a call on Thursday ("Push to Call")
callers <- dreamers_linked%>% 
  mutate(day_called=as.Date(start_time, format= '%Y-%m-%d'))%>%
  select(start_time, day_called, handset_number)

Days_called<- callers %>%
  mutate(
    phone2 = handset_number,
    citizenshipday=ifelse (day_called =="2017-09-17",1,0),
    pushcall=ifelse(day_called<="2017-09-07", 1,0),
    Both=ifelse((citizenshipday==1 & pushcall==1),1,0))

#see if people who made a call for the first time on Citizenship Day, did any actions during the week, join to week of actions table
callers_actions <- counts%>%
  inner_join(Days_called, by ='phone2')%>%
  mutate(
    actionnotcall=ifelse(Count>1, 1,0)
  )%>%
  filter(pushcall==1)

write.csv(dreamers_linked, 'Desktop/Dreamers linked.csv')
write.csv(dreamers_linked_nooutliers, 'Desktop/Dreamers age no outliers.csv')
write.csv(day1, 'Desktop/Week of Actions/Day1.csv')
write.csv(day2, 'Desktop/Week of Actions/Day2.csv')
write.csv(day3, 'Desktop/Week of Actions/Day3.csv')
write.csv(day5, 'Desktop/Week of Actions/Day5.csv')
write.csv(day6, 'Desktop/Week of Actions/Day6.csv')
write.csv(day7, 'Desktop/Week of Actions/Day7.csv')
write.csv(campaigns.DD, 'Desktop/campaignnames.csv')
write.csv(counts, 'Desktop/counts.csv')
write.csv(campaigns.NonDD, 'Desktop/campaignnames_nonDD.csv')
write.csv(states, 'Desktop/states.csv')
write.csv(Days_called, 'Desktop/Days_called.csv')
write.csv(callers_actions, 'Desktop/Citizenship Day callers.csv')

# #Cause types 
# actions<-action%>%
#   mutate(cause_string=as.character(cause_type),
#     animals= ifelse(cause_string=='Animals', 1, 0), 
#     environment=ifelse(cause_string=='Environment',1,0),
#     bully=ifelse(cause_string=='Bullying',1,0),
#     discrimination=ifelse(cause_string=='Discrimination',1,0),
#     homeless=ifelse(cause_string=='Homelessness',1,0),
#     education=ifelse(cause_string=='Education',1,0),
#     menthealth=ifelse(cause_string=='Mental Health',1,0),
#     physhealth=ifelse(cause_string=='Physical Health',1,0),
#     relationships=ifelse(cause_string=='Relationships',1,0),
#     poverty=ifelse(cause_string=='Poverty',1,0),
#     violence=ifelse(cause_string=='Violence',1,0),
#     action_space=ifelse(action_type=='Improve a Space',1,0),
#     action_make=ifelse(action_type=='Make Something',1,0),
#     action_face=ifelse(action_type=='Face to Face',1,0),
#     action_host=ifelse(action_type=='Host An Event',1,0),
#     action_share=ifelse(action_type=='Share Something',1,0),
#     action_donate=ifelse(action_type=='Donate Something',1,0),
#     action_stand=ifelse(action_type=='Take a Stand',1,0),
#     action_start=ifelse(action_type=='Start Something',1,0))

# allDays <-
#   read_csv('Desktop/DACA actions/defend_dreamers_week_of_action_day_1_clicks_20170926_125513.csv')%>%
#   mutate(actionFlag = 1, phone = cleanPhone(phone))%>%
#   filter(!is.na(phone))%>%
#   select(phone, actionFlag)%>%
#   bind_rows(
#     read_csv('Desktop/Week of Actions/Day2.csv')%>%
#       filter(!is.na(phone2))%>%
#       mutate(actionFlag = 2, phone=phone2)%>%
#       select(phone, actionFlag)%>%
#   bind_rows(
#     read_csv('Desktop/DACA actions/daca_day_3_dreamers_are_welcome_signs_clicks_20170926_125413.csv')%>%
#      mutate(actionFlag = 3, phone = cleanPhone(phone))%>%
#      filter(!is.na(phone))%>%
#      select(phone, actionFlag)%>%
#   bind_rows(
#       read_csv('Desktop/DACA actions/daca_day_5_aclu_email_clicks_20170926_125344.csv')%>%
#         mutate(actionFlag = 5, phone = cleanPhone(phone))%>%
#         filter(!is.na(phone))%>%
#         select(phone, actionFlag)%>%
#   bind_rows(
#       read_csv('Desktop/DACA actions/daca_day_6_facebook_post_clicks_20170926_125129.csv')%>%
#         mutate(actionFlag = 6, phone = cleanPhone(phone))%>%
#         filter(!is.na(phone))%>%
#         select(phone, actionFlag)%>%
#   bind_rows(
#      read_csv('Desktop/Dreamers linked.csv')%>%
#         mutate(actionFlag = 7, phone = handset_number))%>%
#         filter(!is.na(handset_number))%>%
#         select(phone, actionFlag)
# ))))



#Merge all data files and link based on phone number
# woa <-day1 %>%
#   left_join(day2, by = 'phone2')%>%
#   left_join(day3, by = 'phone2')%>%
#   left_join(day5, by = 'phone2')%>%
#   left_join(day6, by = 'phone2')%>%
#   left_join(day7, by = 'phone2')%>%
#   mutate(total_actions= action1+ action2 + action3 + action5 +action6 +action7)
# 
# woa <- full_join(list(day1, day2, day3, day5, day6, day7), by = 'phone2')

# woa%>%
#   group_by(total_actions)%>%
#   summarise(
#     Count = n())