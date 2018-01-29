source('config/init.R')
source('config/mySQLConfig.R')
source('Q4/Scripts/customFunctions.R')


# Get survey data ------------------------------------------------------------

nps.q4 <- mungeNPSQ4()

# Get population breakdown -----------------------------------------------------

q <- paste0("
  SELECT
   	count(*) AS total_active,
	  sum(CASE WHEN (((u.customer_io_subscription_status IS NULL) OR u.customer_io_subscription_status = 'subscribed'))
            AND ((u.email IS NULL OR LENGTH(u.email ) = 0 )) AND ((u.mobile IS NOT NULL)) AND (u.sms_status = 'active')
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

# survey breakdown --------------------------------------------------------

nps.q4 %>% count(group) %>% mutate(p = n/sum(n))

# attach population proportions -------------------------------------------

nps <-
  nps.q4 %>%
  left_join(pop)

# Simulate ----------------------------------------------------------------

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

s <-
  nps %>%
  filter(group=='sms_only')
