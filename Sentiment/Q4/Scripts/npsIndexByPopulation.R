source('config/init.R')
source('config/mySQLConfig.R')
source('Q4/Scripts/customFunctions.R')
library(scales)

nps.q4 <- mungeNPSQ4()

#Query generated using paste to insert parameters
q <- paste0("
  SELECT
   	count(*) AS total_active,
	  sum(CASE WHEN sms_status='active' AND
            (u.customer_io_subscription_status <> 'subscribed' OR
            u.customer_io_subscription_status IS NULL)
            THEN 1 ELSE 0 END) AS sms_only,
            sum(CASE WHEN source='niche' THEN 1 ELSE 0 END) AS niche
  FROM quasar.users u
  WHERE (u.sms_status = 'active' OR
      u.customer_io_subscription_status = 'subscribed')
  AND u.email NOT like '%dosomething.org%'"
)

qres <- runQuery(q, which = 'mysql')

pop <-
  qres %>%
  mutate(
    other = total_active-sms_only-niche
  ) %>%
  melt() %>%
  filter(variable!='total_active') %>%
  setNames(c('group','n')) %>%
  mutate(p=n/sum(n))

nps.q4 %>% count(group) %>% mutate(p = n/sum(n))

nps <-
  nps.q4 %>%
  left_join(pop)


scores <- numeric()
for (i in 1:10000) {
  samp <-
    nps %>%
    sample_n(nrow(.), weight = p, replace = T)
  score <- getNPS(samp$nps,11)
  scores <- c(scores, score)
}
scores <- tibble(score = scores)
mean(scores$score)
ggplot(scores, aes(x=score)) + geom_density()
