library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

#Import .csv created from 'Amplify Advocate (Suspended vs Uncover)' SQL query
suspended_uncover<-read_csv('~/Documents/Data Requests/Suspended vs Uncover.csv')

#Number of people participating in Suspended for What - Amplify
xtabs(~participated_in_suspended_amplify, data=suspended_uncover)

#Number of people who signed up for Suspended for What - Advocate
xtabs(~participated_in_suspended_advocate, data=suspended_uncover)

#Number of people participating in Uncover the Truth - Amplify
xtabs(~participated_in_uncover_amplify, data=suspended_uncover)

#Number of people who signed up for Uncover the Truth - Advocate
xtabs(~participated_in_uncover_advocate, data=suspended_uncover)

#########################################################################
####################### SUSPENDED FOR WHAT ##############################
#########################################################################


#Average no. sign ups for other campaigns after signing up for Suspended for What - Amplify
suspended_amplify_averagesignups <- suspended_uncover %>%
  group_by(participated_in_suspended_amplify) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()

#Average no. sign ups for other campaigns after signing up for Suspended for What - Advocate
suspended_advocate_averagesignups <- suspended_uncover %>%
  group_by(participated_in_suspended_advocate) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()


#Average number of signs ups for other campaigns after signing up + reporting back for Suspended for What - Amplify
suspended_reportedback_amplify_averagesignups <- suspended_uncover %>%
  group_by(reportedback_suspended_amplify) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()

#Average number of signs ups for other campaigns after signing up + reporting back for Suspended for What - Advocate
suspended_reportedback_amplify_averagesignups <- suspended_uncover %>%
  group_by(reportedback_suspended_advocate) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()

#########################################################################
####################### UNCOVER THE TRUTH ###############################
#########################################################################

#Average no. sign ups for other campaigns after signing up for Uncover the Truth - Amplify
uncover_amplify_averagesignups <- suspended_uncover %>%
  group_by(participated_in_uncover_amplify) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()

#Average no. sign ups for other campaigns after signing up for Uncover the Truth- Advocate
uncover_advocate_averagesignups <- suspended_uncover %>%
  group_by(participated_in_uncover_advocate) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()


#Average number of signs ups for other campaigns after signing up + reporting back for Uncover the Truth - Amplify
uncover_reportedback_amplify_averagesignups <- suspended_uncover %>%
  group_by(reportedback_uncover_amplify) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()

#Average number of signs ups for other campaigns after signing up + reporting back for Uncover the Truth  - Advocate
uncover_reportedback_amplify_averagesignups <- suspended_uncover %>%
  group_by(reportedback_uncover_advocate) %>%
  summarise(
    mean_signups = mean(total_campaigns_post_march),
  ) %>% print()

