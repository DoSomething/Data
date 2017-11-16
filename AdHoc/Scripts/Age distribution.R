library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)

Age<-DOB%>%
  mutate(
    birthdate = as.Date(users.birthdate_date),
    daysOld = Sys.Date() - birthdate,
    ageToday = as.numeric(floor(daysOld / 365.25))
  ) 

ggplot(Age, aes(x=ageToday)) + geom_density()
