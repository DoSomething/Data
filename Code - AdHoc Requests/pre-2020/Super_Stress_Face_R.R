library(tidyverse)
library(data.table)
library(plyr)
library(dplyr)

groupB<-read.csv('~/Desktop/Show Stress B group.csv')
groupB$group<-"groupB"
control<-read.csv('~/Desktop/Show Stress Face Control.csv')
control$group<-"control"

showstress<-rbind(groupB,control)

showstress<-showstress%>%
  mutate(
    reportedback=ifelse(total_rbs>0,'yes','no'),
    visitedsite=ifelse(site_visits>0, 'yes', 'no')
  )

#Percentage reporting back and visitnig site
CrossTable(showstress$group, showstress$reportedback, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(showstress$group, showstress$visitedsite, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

# #Averages
# ddply(showstress, .(group), summarize, rb_mean=mean(total_rbs), site_visits_mean=mean(site_visits))

#Average number of reportbacks
group_by(showstress, group) %>%
  summarise(
    count = n(),
    mean = mean(total_rbs, na.rm = TRUE),
    sd = sd(total_rbs, na.rm = TRUE)
  )

#Average number of site visits
group_by(showstress, group) %>%
  summarise(
    count = n(),
    mean = mean(site_visits, na.rm = TRUE),
    sd = sd(site_visits, na.rm = TRUE)
  )

#T-tests for statistical significance
t.test(showstress$total_rbs~showstress$group)
t.test(showstress$site_visits~showstress$group)

#Total number of reportbacks
aggregate(showstress$total_rbs, by=list(Category=showstress$group), FUN=sum)

#Total number of site visits
aggregate(showstress$site_visits, by=list(Category=showstress$group), FUN=sum)
