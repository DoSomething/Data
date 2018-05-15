library(tidyverse)
library(data.table)
library(plyr)
library(concatenate)
source('config/init.R')
source('config/mySQLConfig.R')

#import Census ZCTA relationship data file (https://www.census.gov/geo/maps-data/data/ua_rel_download.html)
ua_zcta_rel_10 <- read.csv("~/Documents/Data Requests/Rural vs suburban/ua_zcta_rel_10.txt")

census <- ua_zcta_rel_10%>%
  # select(UANAME, ZCTA5)%>%
  mutate(UANAME = as.character(UANAME),
         ZCTA5 = as.character(ZCTA5))

#define rural areas 
census <- census%>%
  select(UANAME, ZCTA5, POPPT)%>%
  mutate(
    rural =ifelse(UANAME == 'Not in a 2010 urban area', 1,0),
    urban =ifelse(UANAME == 'Not in a 2010 urban area', 0,1))

# check against Census figures (249m urban, 59m rural)
census %>% group_by(rural) %>% summarise(sum(POPPT))

census <- census%>%
  rename(zip = ZCTA5)

#Pull zipcodes for all subscribed members
q_address<- paste0("SELECT distinct u.northstar_id,
                u.addr_zip,
                u.addr_city,
                u.addr_state
                FROM quasar.users u
                WHERE ((u.addr_zip IS NOT NULL AND LENGTH(u.addr_zip ) <> 0 ) AND (u.addr_zip IS NOT NULL)) AND (u.customer_io_subscription_status = 'subscribed' OR u.sms_status = 'active')")

members_address <- runQuery(q_address, which = 'mysql')

members_address <-members_address%>%
  rename(zip = addr_zip)

#Concatenate city and state
members_address$NAME <- paste(members_address$addr_city, members_address$addr_state, sep =", ")

#Join ds member data with census by zcta data
area <-left_join(members_address,census, by = "zip")

#sort northstar ids and rural definition
attach(area)
area_sorted <- area[order(northstar_id, zip, rural),]

#select first case (urban) for those with multiple UANAMEs for zcta (assume they're from city)
area_nodup <- area_sorted%>%
filter(!duplicated(northstar_id))

#Counts on rural members
area_nodup%>%count(rural)%>%mutate(p=n/sum(n))


