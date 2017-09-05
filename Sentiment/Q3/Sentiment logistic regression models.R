library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(rpart)
library(rpart.plot)
library(QuantPsyc)

install.packages("QuantPsyc")

#Logistic regression model - Outcome = being a promoter, predictors are satisfaction on touchpoints
logreg_promoter <-glm(promoter~communication_rec + causes_five + website_five + campaigns_five + scholarship_five + community_five,
                      data=Sentiment_Survey_Q3_clean_, family=binomial(link="logit"))

summary(logreg_promoter)
exp(coef(logreg_promoter))

#Logistic regression model - Outcome = being a persuadable
logreg_persuadable <-glm(Age_rec +persuadable~communication_rec + causes_five + website_five + campaigns_five + scholarship_five + community_five,
                         data=Sentiment_Survey_Q3_clean_, family=binomial())

summary(logreg_persuadable)
exp(coef(logreg_persuadable))

#Logistic regression model - Outcome = Re-engagement in next month
logreg_reengage <-glm(reengage~communication_rec + causes_five + website_five + campaigns_five + scholarship_five + community_five,
                      data=Sentiment_Survey_Q3_clean_, family=binomial())

summary(logreg_reengage)
exp(coef(logreg_reengage))

#Linear regression (outcome = NPS)
fit <-lm(NPS_rec ~ Communication + Causes + Website + Campaigns + Scholarships + Community,data=Sentiment_Survey_Q3_clean_)

summary(fit)
#Standardized betas
lm.beta(fit)



#Decision Tree - 80/20 split
Sentiment_Survey_Q3_clean_ %<>%
  tbl_dt() %>%
  mutate(
    random=runif(nrow(Sentiment_Survey_Q3_clean_), 0, 1),
    group = ifelse(random > .8, 'out', 'in')
    ) 

#Decision tree for being a promoter based on satisfaction on touchpoints
promoters <-rpart(
      as.factor(promoter) ~ 
        Causes + Communication + Website + Campaigns + Scholarships + Community, 
        data = Sentiment_Survey_Q3_clean_ [group=='in']
        , cp=0.01)

rpart.plot(promoters, main="promoters")

#Decision tree for being a persuadable based on satisfaction on touchpoints
persuadables <-rpart(
  as.factor(persuadable) ~ 
    Causes + Communication + Website + Campaigns + Scholarships + Community, 
  data = Sentiment_Survey_Q3_clean_ [group=='in']
  , cp=0.01)

rpart.plot(persuadables, main="persuadables")

#Decision tree for being a detractor based on satisfaction on touchpoints
detractors <-rpart(
  as.factor(detractor) ~ 
    Causes + Communication + Website + Campaigns + Scholarships + Community, 
  data = Sentiment_Survey_Q3_clean_ [group=='in']
  , cp=0.01)

rpart.plot(detractors, main="detractors")

#Age distribution
ggplot(Sentiment_Survey_Q3_clean_, aes(x=Age_rec)) + geom_density()
