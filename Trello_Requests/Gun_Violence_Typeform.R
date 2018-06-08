source('config/init.R')
library(httr)
library(jsonlite)
library(gmodels)
library(dplyr)
library(tibble)
library(RMySQL)
library(dplyr)
library(data.table)

### Grab API key from environment var
key <- Sys.getenv('TYPEFORM_KEY')

# Get a list of all our typeforms and their unique IDs
allForms <- paste0('https://api.typeform.com/v1/forms?key=',key)
res <- GET(url = allForms)
json <- httr::content(res, as = "text")
allTypeForms <- fromJSON(json)

# Grab the ID for the DoSomething Smoking survey
allTypeForms %>%
  filter(name == 'Gun Violence Response - 5/21') %>%
  select(id) %>% as.character() -> feedback

smsq1Key <- 'zi8Lzs'

# Submit request for that survey
gun_control<- paste0('https://api.typeform.com/v1/form/',smsq1Key,'?key=',key)
res <- GET(url = gun_control)
json <- content(res, as = "text")
feedbackResults <- fromJSON(json)

# Grab questions and answers (including hidden fields) from request
questions <- as.tibble(feedbackResults$questions)
answers <- as.tibble(cbind(feedbackResults$responses$hidden, feedbackResults$responses$answers))


gun_violence <- read.csv('~/Downloads/Gun Violence Response - 5_21.csv')
gun_violence <- gun_violence%>%
    rename(northstar_id = id)

a<-prepQueryObjects(gun_violence$northstar_id)

gun_violence <- paste0("
SELECT ca.northstar_id,
      extract(year FROM (from_days(dateDiff(current_timestamp,u.birthdate)))) AS age,
      count(DISTINCT ca.signup_id) AS total_signups
      FROM quasar.campaign_activity ca
      LEFT JOIN quasar. users u
      ON ca.northstar_id = u.northstar_id
      WHERE u.northstar_id IN", a,
      "GROUP BY u.northstar_id")


members_gunviolence <- runQuery(gun_violence, which = 'mysql')

merged<-merge(x=gun_violence, y=members_gunviolence, by ="northstar_id", all=TRUE)

merged<-merged%>%
  mutate(under_18=ifelse(age<18,1,0))

CrossTable(merged$s, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$age, prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

CrossTable(merged$Registering.people.to.vote.to.elect.officials.who.support.common.sense.gun.laws, merged$under_18,prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

write.csv(merged, file = 'gen violence.csv')
