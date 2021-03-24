#Load libraries
source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

#Set random number generator seed so same observations are picked every time
set.seed(57)
prodrun=T

#Changed these paramaters that are usually inserted into query. Removed age paramaters because they were only used for Niche members in past quarters
#yearAgo <- Sys.Date() - 365
#age13 <- Sys.Date() - (365.25*13)
#age25 <- Sys.Date() - (365.25*25)

#Identify members who answered Q2 NPS from Member Survey
mem_survey <- "select *
        from survey.member_survey_2018"

member_survey_2018 <- runQuery(mem_survey)%>%
  mutate(nsid=as.character(nsid))%>%
  filter(!is.na(nsid))

#Pull data from db - invite members who signed up for any campaign in the last year
q3_drawsample <- glue_sql("
            SELECT u.northstar_id, u.email, u.cio_status,u.sms_status,
              CASE WHEN(u.sms_status IN ('active', 'less', 'pending') AND (u.cio_status <> 'customer_subscribed' OR u.cio_status IS null))
              OR ((u.email IS NULL OR LENGTH(u.email ) = 0 )) OR (u.email like '%mobile.import%') THEN 1 ELSE 0 end AS sms_only,
              c.signup_created_at
            FROM public.users u
            INNER JOIN public.campaign_activity c ON c.northstar_id = u.northstar_id
            WHERE ((u.sms_status IN ('active', 'pending', 'less') OR u.cio_status = 'subscribed')
            AND u.country = 'US'
            AND (u.email NOT like '%dosomething.org%' OR u.email NOT like '%runscope%' OR ((u.email IS NULL OR LENGTH(u.email ) = 0 )))
            AND c.signup_created_at >= now() - interval ' 1 year'
            AND u.northstar_id NOT IN ({nsids*}))
            GROUP BY 1,2,3,4,5,6",
            nsids = member_survey_2018$nsid,
            .con = pg
)

q3_2018_drawsample <- runQuery(q3_drawsample)

#Get data
#It is at the NSID-Campaign signup level
if (prodrun==T) {
  qres <- runQuery(q3_drawsample)
  save(q3_2018_drawsample, file='Q3_2018/Data/sampleQueryResult.RData')
} else {
  load('Q3_2018/sampleQueryResult.RData')
}

#Get earliest signup date
minDate <- min(as.Date(substr(qres$signup_created_at, 1, 10)))

#Group by northstar and get the average signup date per northstar
#This required removing the time component with substr, turning it into a date,
#then turning it into a number to be able to take the mean, then turning it
#back into a date
dat <-
  qres %>%
  group_by(northstar_id) %>%
  summarise(
    sms_only = max(sms_only),
    avg_signup_date = as.Date(mean(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01'),
    first_signup_date = as.Date(min(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01'),
    last_signup_date = as.Date(max(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01')
  ) %>%
  #This places the signup dates on a 1:max axis
  #scalerange converts that into a var that ranges from 0:1
  mutate(
    dateCounter = as.numeric(avg_signup_date) - as.numeric(minDate) + 1,
    scaleDates = scalerange(dateCounter),
    twoMonthsAgo=Sys.Date()-60,
    twoweeks=Sys.Date()-14
  )

# #Calculate the point that we want to be the fattest part of the distribution
# peak <-
#   dat %>%
#   filter(avg_signup_date==twoMonthsAgo) %>%
#   dplyr::select(scaleDates) %>% unique() %>% as.numeric()

#Remove very recent signup folks (two weeks ago)
# dat %<>%
#   filter(first_signup_date < twoMonthsAgo) %>%
#   #Likelihood of being selected is 1 - the distance from the peak so the closer
#   #it is to the peak, the more likely it is to be selected
#   mutate(
#     prob = 1 - abs(scaleDates - peak)
#   ) %>%
#   mutate(id=northstar_id)

#Now we draw samples by filtering to the group of interest, tell it how many
#to draw, don't sample anyone more than once, and weight it by the prob
# sampleSMS <-
#   dat %>%
#   filter(sms_only==1) %>%
#   sample_n(20000, replace = F, weight = prob)
# saveCSV(select(sampleSMS, id))
# ggplot(sampleSMS, aes(x=avg_signup_date)) +
#   geom_density() + ggtitle('SMS Only') +
#   scale_x_date(breaks=pretty_breaks(15))
#
# sampleRegular <-
#   dat %>%
#   filter(sms_only==0) %>%
#   sample_n(55000, replace = F, weight = prob)
# saveCSV(select(sampleRegular, id))
# ggplot(sampleRegular, aes(x=avg_signup_date)) +
#   geom_density() + ggtitle('Regular') +
#   scale_x_date(breaks=pretty_breaks(15))

###########################################################
#################### Q3 SAMPLING ##########################
###########################################################

#Look at counts by months since average signup date
#Create month categories
Signups_month <-dat%>%
    mutate(time_since_avgsignup = Sys.Date() - avg_signup_date,
           time_since_last_signup = Sys.Date() - last_signup_date,
           id=northstar_id,
           avg_month_since_signup=
            case_when(time_since_avgsignup <61 ~ '2 months ago',
                      time_since_avgsignup >60 & time_since_avgsignup <91 ~ '3 months ago',
                      time_since_avgsignup >90 & time_since_avgsignup <121 ~ '4 months ago',
                      time_since_avgsignup >120 & time_since_avgsignup <151 ~ '5 months ago',
                      time_since_avgsignup >150 & time_since_avgsignup <181 ~ '6 months ago',
                      time_since_avgsignup >180 & time_since_avgsignup <211 ~ '7 months ago',
                      time_since_avgsignup >210 & time_since_avgsignup <241 ~ '8 months ago',
                      time_since_avgsignup >240 & time_since_avgsignup <271 ~ '9 months ago',
                      time_since_avgsignup >270 & time_since_avgsignup <311 ~ '10 months ago',
                      time_since_avgsignup >310 & time_since_avgsignup <341 ~ '11 months ago',
                      time_since_avgsignup >340 ~ '12 months ago'))

#Create groups for SMS-only and Regular invites
Signups_Regular <-Signups_month%>%
  filter(sms_only==0)

Signups_SMS <-Signups_month%>%
  filter(sms_only==1)

#Look at counts of months since average signup date by group
CrossTable(Signups_month$avg_month_since_signup, Signups_month$sms_only, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, missing.include = FALSE,format= c("SPSS"))

#We want a total of 55k Regular invites.
#Sample ALL 6 months avg signups from Regular members (25k) because of low signups during Q3 - the other half should have an average signup date in the last 6 months, filter our news signups in the last 2 weeks.
Signups_Regular_sample <-Signups_Regular%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup <181)
ggplot(Signups_Regular_sample, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Regular Signups in last 6 months') +
  scale_x_date(breaks=pretty_breaks(15))
#Randomly select the remaining half who had avg signup dates that were 6-12 months ago
Signups_Regular_sample_older <-Signups_Regular%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup>180)%>%
  sample_n(30000, replace = F)
ggplot(Signups_Regular_sample_older, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Regular Signups after 6 months') +
  scale_x_date(breaks=pretty_breaks(15))

#Join before 6m and after 6m samples
Regular_sample <-rbind(Signups_Regular_sample_older,Signups_Regular_sample)
saveCSV(select(Regular_sample, id))
#plot these
ggplot(Regular_sample, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Regular Signups Invites') +
  scale_x_date(breaks=pretty_breaks(15))
#look at breakdown
Regular_sample_counts <- Regular_sample%>%
  select(avg_month_since_signup,northstar_id)%>%
  count(avg_month_since_signup)%>%
  mutate(p=n/sum(n))%>%
  print()

##########################################################
################## SMS Only Sampling #####################
#########################################################

#We want a total of 20k SMS-only invites and want to keep consistent with the breakdown of invites for Regular members.
#Calculate goal sizes for each month category based on breakdown from Regular invites above
SMS_sample_counts <- Regular_sample_counts%>%
  mutate(goal_size=p*20000)

#Randomly select sample sizes from above to match % of Regular invites
Signups_SMS_sample_2m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup <61)%>%
  sample_n(1536, replace = F)
Signups_SMS_sample_3m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >60 & time_since_avgsignup<91)%>%
sample_n(1470, replace = F)
Signups_SMS_sample_4m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >90 & time_since_avgsignup<121)%>%
sample_n(1861, replace = F)
Signups_SMS_sample_5m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >120 & time_since_avgsignup<151)%>%
sample_n(1101, replace = F)
Signups_SMS_sample_6m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >150 & time_since_avgsignup<181)%>%
sample_n(3155, replace = F)
Signups_SMS_sample_7m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >180 & time_since_avgsignup<211)%>%
sample_n(1350, replace = F)
Signups_SMS_sample_8m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >210 & time_since_avgsignup<241)%>%
sample_n(2172, replace = F)
Signups_SMS_sample_9m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >240 & time_since_avgsignup<271)%>%
sample_n(1767, replace = F)
Signups_SMS_sample_10m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >270 & time_since_avgsignup<311)%>%
sample_n(2213, replace = F)
Signups_SMS_sample_11m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >310 & time_since_avgsignup<341)%>%
sample_n(1484, replace = F)
Signups_SMS_sample_12m <-Signups_SMS%>%
  filter(first_signup_date < twoweeks & time_since_avgsignup >340)%>%
sample_n(1887, replace = F)

SMS_sample <-rbind(Signups_SMS_sample_2m,Signups_SMS_sample_3m,Signups_SMS_sample_4m,Signups_SMS_sample_5m,Signups_SMS_sample_6m,
      Signups_SMS_sample_7m,Signups_SMS_sample_8m,Signups_SMS_sample_9m,Signups_SMS_sample_10m,Signups_SMS_sample_11m,
      Signups_SMS_sample_12m)
saveCSV(select(SMS_sample, id))

#plot these to see if it matches Regular invites
ggplot(SMS_sample, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('SMS Signups Invites') +
  scale_x_date(breaks=pretty_breaks(15))
#look at breakdown
SMS_sample_counts <- SMS_sample%>%
  select(avg_month_since_signup,northstar_id)%>%
  count(avg_month_since_signup)%>%
  mutate(p=n/sum(n))%>%
  print()

#######################################
############# WEIGHTS ################
#######################################
#Sample 50% who had average signup date as 2 months ago
# pop_weights <- pop%>%
#   mutate(pct_forweight=
#            case_when(avg_month_since_signup=='2 months ago' ~ 0.50,
#                      TRUE ~ 0.05))
#
# pop_weights <- pop_weights%>%
#   mutate(weight=1/(p/pct_forweight))
#
# #join with data
# dat_weights <- Signups_month%>%
#   left_join(pop_weights, Signups_month, by ="avg_month_since_signup")
#
#
# #to draw, don't sample anyone more than once, and weight it by the prob
# sampleSMS <-
#   dat_weights %>%
#   filter(sms_only==1) %>%
#   sample_n(20000, replace = F, weight = weight)
# saveCSV(select(sampleSMS, id))
# ggplot(sampleSMS, aes(x=avg_signup_date)) +
#   geom_density() + ggtitle('SMS Only') +
#   scale_x_date(breaks=pretty_breaks(15))
#
#
# sampleRegular <-
#   dat_weights %>%
#   filter(sms_only==0) %>%
#   sample_n(55000, replace = F, weight = weight)%>%
# saveCSV(select(sampleRegular, id))
# ggplot(sampleRegular, aes(x=avg_signup_date)) +
#   geom_density() + ggtitle('Regular') +
#   scale_x_date(breaks=pretty_breaks(15))

#Misc scripts
# Signup_Regular_counts <- Signups_Regular%>%
#   select(avg_month_since_signup,northstar_id)%>%
#   count(avg_month_since_signup)%>%
#   mutate(p=n/sum(n))%>%
#   print()

# Signup_SMS_counts <- Signups_SMS%>%
#   select(avg_month_since_signup,northstar_id)%>%
#   count(avg_month_since_signup)%>%
#   mutate(p=n/sum(n))%>%
#   print()



