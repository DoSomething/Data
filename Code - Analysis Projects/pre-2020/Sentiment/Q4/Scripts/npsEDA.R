source('config/init.R')
source('config/mySQLConfig.R')
source('Q4/Scripts/customFunctions.R')
library(scales)
library(caret)

nps.q4 <- mungeNPSQ4()

q4nsids <- prepQueryObjects(nps.q4$northstar_id)

q <-
  paste0(
    "SELECT
      u.northstar_id,
      FLOOR(DATEDIFF(CURRENT_DATE(), u.birthdate) / 365) age,
      DATEDIFF(date(now()), date(u.created_at)) as 'days_a_member',
      CASE WHEN u.source = 'niche'
      THEN 1 ELSE 0 END AS niche,
      CASE WHEN (u.customer_io_subscription_status IS NULL
        OR u.customer_io_subscription_status = 'subscribed')
        AND (u.email IS NULL OR LENGTH(u.email) = 0)
        AND u.mobile IS NOT NULL
        AND u.sms_status = 'active'
      THEN 1 ELSE 0 END AS sms_only,
      count(DISTINCT c.signup_id) AS total_signups,
      SUM(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) AS total_rbs,
      count(DISTINCT ul.last_accessed) as 'site_visits'
    FROM quasar.users u
    INNER JOIN quasar.campaign_activity c ON c.northstar_id = u.northstar_id
    LEFT JOIN quasar.users_log ul
      ON u.northstar_id=ul.northstar_id
    WHERE u.northstar_id IN ", q4nsids,
    "GROUP BY u.northstar_id"
  )

qres <- runQuery(q, which = 'mysql')

surveyRes <-
  read_csv('Q4/Data/Q4 All_Sentiment_surveys.csv') %>%
  select(northstar_id, ds_value) %>%
  mutate(
    value =
      case_when(
        grepl('Scholarship', ds_value) ~ 'Scholarship',
        grepl('difference', ds_value) ~ 'Fun Diff',
        grepl('larger', ds_value) ~ 'Part Larger',
        grepl('Resources', ds_value) ~ 'Resources'
      )
    )

nps.eda <-
  qres %>%
  left_join(
    nps.q4
  ) %>%
  left_join(
    surveyRes
  ) %>%
  mutate(
    promoter = as.factor(if_else(nps>=10, 'yes', 'no')),
    promoter.n = if_else(nps>=10, 1, 0),
    detractor = as.factor(if_else(nps<=7, 'yes', 'no')),
    detractor.n = if_else(nps<=7, 1, 0)
  )

# candidates = c('age','days_a_member','total_signups','value',
#                'total_rbs','site_visits','group')
# promo = as.formula(paste('promoter ~ ',paste(candidates, sep = ' + ', collapse = '+')))

nps.eda <- fillNAs(nps.eda)
#
# ctrl =
#   trainControl(
#     method="repeatedcv",
#     number=10,
#     repeats=2,
#     selectionFunction = "oneSE",
#     classProb=T,
#     summaryFunction = twoClassSummary,
#     savePredictions = TRUE
#   )
#
# rf.p = train(
#   promo,
#   data=nps.eda,
#   method="rf",
#   trControl=ctrl,
#   metric="ROC"
# )

# varImp(rf.p, scale=FALSE) %>% plot(main = 'Variable Importance - Promoter')

# nps.eda$p_promo <- predict(rf.p, nps.eda, type='prob')[2]
candidates = c('age','days_a_member','total_signups','value',
               'site_visits','group')
promo = as.formula(paste('promoter.n ~ ',paste(candidates, sep = ' + ', collapse = '+')))
plog <- glm(promo, 'binomial', nps.eda)
nps.eda$p_promo <- predict(plog, nps.eda, type='response')

detra = as.formula(paste('detractor.n ~ ',paste(candidates, sep = ' + ', collapse = '+')))
dlog <- glm(detra, 'binomial', nps.eda)
nps.eda$p_detract <- predict(dlog, nps.eda, type='response')

forPlot <-
  nps.eda %>%
  select(p_promo, age, days_a_member, total_signups, site_visits) %>%
  melt(id.var='p_promo')

ggplot(forPlot, aes(x=value, y=p_promo)) +
  geom_smooth(method='lm') + facet_wrap(~variable, scale='free_x')

noMissing <- filter(nps.eda, value!='Missing')
ggplot(noMissing, aes(x=value, y=p_promo)) + ylim(0,1) +
  geom_bar(stat = "summary", fun.y = "mean", position='dodge',aes(fill=group))

ggplot(noMissing, aes(x=value, y=detractor.n)) + ylim(0,1) +
  geom_bar(stat = "summary", fun.y = "mean", position='dodge',aes(fill=group))

ggplot(noMissing, aes(x=value, y=p_detract)) + ylim(0,1) +
  geom_bar(stat = "summary", fun.y = "mean", position='dodge',aes(fill=group))

nps.eda %>% group_by(group) %>% summarise(n=n()) %>% mutate(prop=n/sum(n))

# load('~/Downloads/model.rds')
#
# preds <-
#   as.tibble(expand.grid(survey = c('Nonniche','sms','niche'),
#                         action_type_DonateSomething = seq(0,10,1))) %>%
#   arrange(survey, action_type_DonateSomething)
#
# preds$predictions <- predict(model, preds, type='response')
# ggplot(preds, aes(x=action_type_DonateSomething, y=predictions, color=survey)) + geom_line()
