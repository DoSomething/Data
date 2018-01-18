library(tidyverse)
library(data.table)
library(plyr)

competitors<-read.csv('~/Desktop/5 Days Multiple Actions.csv')
competitors$group<-"competitors"
control<-read.csv('~/Desktop/Control.csv')
control$group<-"control"

both<-rbind(competitors,control)

both<-both%>%
  mutate(
    rb_30=ifelse(total_rbs_30>0,'yes','no'),
    rb_60=ifelse(total_rbs_60>0,'yes','no'),
    rb_90=ifelse(total_rbs_90>0,'yes','no')
  )

write.csv(both, file = '5 Days, 5 Actions.csv')

CrossTable(both$group, both$rb_30, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(both$group, both$rb_60, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))
CrossTable(both$group, both$rb_90, prop.c=FALSE, prop.r=TRUE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE, format= c("SPSS"))

ddply(both, .(group), summarize, rb_30_mean=mean(total_rbs_30), rb_60_mean=mean(total_rbs_60), rb_90_mean=mean(total_rbs_90))
