
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
            AND (u.email NOT like '%dosomething.org%' OR ((u.email IS NULL OR LENGTH(u.email ) = 0 )))
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
    first_signup_date = as.Date(min(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01')
  ) %>%
  #This places the signup dates on a 1:max axis
  #scalerange converts that into a var that ranges from 0:1
  mutate(
    dateCounter = as.numeric(avg_signup_date) - as.numeric(minDate) + 1,
    scaleDates = scalerange(dateCounter),
    twoMonthsAgo=Sys.Date()-60,
    twoweeks=Sys.Date()-14
  )

#Calculate the point that we want to be the fattest part of the distribution
peak <-
  dat %>%
  filter(avg_signup_date==twoMonthsAgo) %>%
  dplyr::select(scaleDates) %>% unique() %>% as.numeric()

#Remove very recent signup folks (two weeks ago)
dat %<>%
  filter(first_signup_date < twoweeks) %>%
  #Likelihood of being selected is 1 - the distance from the peak so the closer
  #it is to the peak, the more likely it is to be selected
  mutate(
    prob = 1 - abs(scaleDates - peak)
  ) %>%
  mutate(id=northstar_id)

#Now we draw samples by filtering to the group of interest, tell it how many
#to draw, don't sample anyone more than once, and weight it by the prob
sampleSMS <-
  dat %>%
  filter(sms_only==1) %>%
  sample_n(20000, replace = F, weight = prob)
saveCSV(select(sampleSMS, id))
ggplot(sampleSMS, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('SMS Only') +
  scale_x_date(breaks=pretty_breaks(15))

sampleRegular <-
  dat %>%
  filter(sms_only==0) %>%
  sample_n(55000, replace = F, weight = prob)
saveCSV(select(sampleRegular, id))
ggplot(sampleRegular, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Regular') +
  scale_x_date(breaks=pretty_breaks(15))
