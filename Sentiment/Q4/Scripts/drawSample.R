#Load libraries
source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

#Set random number generator seed so same observations are picked every time
set.seed(57)

#Paramaters to be inserted into query
yearAgo <- Sys.Date() - 365
age13 <- Sys.Date() - (365.25*13)
age25 <- Sys.Date() - (365.25*25)

#Query generated using paste to insert parameters
q <- paste0("
  SELECT
    u.northstar_id,
    CASE WHEN u.northstar_id_source_name = 'niche' THEN 1 ELSE 0 END as niche,
    CASE WHEN u.moco_current_status = 'active'
        AND (u.customer_io_subscription_status <> 'subscribed' OR
             u.customer_io_subscription_status IS NULL)
        THEN 1 ELSE 0 END AS sms_only,
    c.signup_created_at
  FROM quasar.users u
  INNER JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
  WHERE (u.moco_current_status = 'active' OR
      u.customer_io_subscription_status = 'subscribed')
  AND u.country = 'US'
  AND u.email NOT like '%dosomething.org%'
  AND c.signup_created_at >= '",yearAgo,"'
  AND u.birthdate >= '", age25, "'
  AND u.birthdate < '", age13,"'"
)

#Get data
#It is at the NSID-Campaign signup level
qres <- runQuery(q, which = 'mysql')

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
    niche = max(niche),
    sms_only = max(sms_only),
    avg_signup_date = as.Date(mean(as.numeric(as.Date(substr(signup_created_at, 1, 10)))), origin='1970-01-01')
  ) %>%
  #This places the signup dates on a 1:max axis
  #scalerange converts that into a var that ranges from 0:1
  mutate(
    dateCounter = as.numeric(avg_signup_date) - as.numeric(minDate) + 1,
    scaleDates = scalerange(dateCounter)
  )

#Calculate the point that we want to be the fattest part of the distribution
peak <-
  dat %>%
  filter(avg_signup_date==Sys.Date()-60) %>%
  select(scaleDates) %>% unique() %>% as.numeric()

#Remove very recent signup folks
dat %<>%
  filter(avg_signup_date < '2017-11-01') %>%
  #Likelihood of being selected is 1 - the distance from the peak so the closer
  #it is to the peak, the more likely it is to be selected
  mutate(
    prob = 1 - abs(scaleDates - peak)
  ) %>%
  mutate(id=northstar_id)

#Now we draw samples by filtering to the group of interest, tell it how many
#to draw, don't sample anyone more than once, and weight it by the prob
sampleNiche <-
  dat %>%
  filter(niche==1) %>%
  sample_n(24000, replace = F, weight = prob)
saveCSV(select(sampleNiche, id))
#Plot to visualize what we ended up with
ggplot(sampleNiche, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Niche') +
  scale_x_date(breaks=pretty_breaks(15))

#Rinse and Repeat
sampleSMS <-
  dat %>%
  filter(sms_only==1 & !(northstar_id %in% sampleNiche$northstar_id)) %>%
  sample_n(12000, replace = F, weight = prob)
saveCSV(select(sampleSMS, id))
ggplot(sampleSMS, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('SMS Only') +
  scale_x_date(breaks=pretty_breaks(15))

sampleRegular <-
  dat %>%
  filter(niche==0 & sms_only==0) %>%
  sample_n(20000, replace = F, weight = prob)
saveCSV(select(sampleRegular, id))
ggplot(sampleRegular, aes(x=avg_signup_date)) +
  geom_density() + ggtitle('Regular') +
  scale_x_date(breaks=pretty_breaks(15))