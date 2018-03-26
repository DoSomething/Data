q <- "SELECT 
u.birthdate
FROM quasar.campaign_activity c
INNER JOIN quasar.users u ON c.northstar_id = u.northstar_id
WHERE c.campaign_run_id IN (7060,7944)"

vcard <- runQuery(q, 'mysql')

bd <- 
  vcard %>% 
  mutate(
    birthdate = as.Date(birthdate),
    daysOld = Sys.Date() - birthdate,
    ageToday = as.numeric(floor(daysOld / 365.25))
  ) %>% 
  filter(ageToday > 10 & ageToday < 40)

ggplot(bd, aes(x=ageToday)) + 
  geom_bar(stat='count') + 
  scale_x_continuous(breaks=pretty_breaks(20))
length(which(bd$ageToday<18))/nrow(bd)
