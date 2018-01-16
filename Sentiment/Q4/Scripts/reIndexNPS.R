source('config/init.R')
source('config/mySQLConfig.R')
source('Q4/Scripts/customFunctions.R')
library(scales)

nps.q2 <- mungeNPSQ2()
nps.q3 <- mungeNPSQ3()
nps.q4 <- mungeNPSQ4()
nps.2016 <- mungeNPS2016()

nps <-
  nps.q3 %>%
  bind_rows(nps.q4) %>%
  bind_rows(nps.2016) %>%
  bind_rows(nps.q2)

ggplot(nps, aes(x=nps, color=type)) +
  geom_density() + facet_grid(~group) +
  scale_x_continuous(breaks=pretty_breaks(10))

breakdown <-
  nps %>%
  group_by(type, group) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    proportion = count / sum(count)
  ) %>% arrange(type, group) %>% data.table()

# nps <-
#   nps %>%
#   mutate(
#     sample_weight =
#       case_when(
#         group=='niche' ~ breakdown[group=='niche' & type=='Q4 2017',proportion],
#         group=='other' ~ breakdown[group=='other' & type=='Q4 2017',proportion],
#         group=='sms_only' ~ breakdown[group=='sms_only' & type=='Q4 2017',proportion]
#       )
#   )


# 2016 Calibration --------------------------------------------------------

nicheN <-
  breakdown[group=='niche' & type=='Q4 2017',proportion] * nrow(nps.2016)
otherN <-
  breakdown[group=='other' & type=='Q4 2017',proportion] * nrow(nps.2016)
smsN <-
  breakdown[group=='sms_only' & type=='Q4 2017',proportion] * nrow(nps.2016)
# nps %<>% left_join(probs)
dist2016 <- c()
for (i in 1:1000) {
  nps2016Sample <-
    nps %>%
    filter(type=='2016' & group=='niche') %>%
    sample_n(size = nicheN,replace = T) %>%
    bind_rows(
      nps %>%
        filter(type=='2016' & group=='other') %>%
        sample_n(size = otherN,replace = T)
    ) %>%
    bind_rows(
      nps %>%
        filter(type=='2016' & group=='sms_only') %>%
        sample_n(size = smsN,replace = T)
    )

  nps2016Sample %>% group_by(group) %>% summarise(count=n()) %>% mutate(proportion=count/sum(count))

  x <- getNPS(nps2016Sample$nps, 11)
  dist2016 <- c(x,dist2016)
}
ggplot(data.frame(dist2016), aes(x=dist2016)) +
  geom_density() +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  ggtitle('Recalibrated NPS Distribution - 2016')

mean(dist2016)

# Calibrate Q3 ------------------------------------------------------------

nicheN <-
  breakdown[group=='niche' & type=='Q4 2017',proportion] * nrow(nps.q3)
otherN <-
  breakdown[group=='other' & type=='Q4 2017',proportion] * nrow(nps.q3)
smsN <-
  breakdown[group=='sms_only' & type=='Q4 2017',proportion] * nrow(nps.q3)

dist2017.q3 <- c()
for (i in 1:1000) {
  nps2016Sample <-
    nps %>%
    filter(type=='Q3 2017' & group=='niche') %>%
    sample_n(size = nicheN,replace = T) %>%
    bind_rows(
      nps %>%
        filter(type=='Q3 2017' & group=='other') %>%
        sample_n(size = otherN,replace = T)
    ) %>%
    bind_rows(
      nps %>%
        filter(type=='Q3 2017' & group=='sms_only') %>%
        sample_n(size = smsN,replace = T)
    )

  nps2016Sample %>% group_by(group) %>% summarise(count=n()) %>% mutate(proportion=count/sum(count))

  x <- getNPS(nps2016Sample$nps, 11)
  dist2017.q3 <- c(x,dist2017.q3)
}
ggplot(data.frame(dist2017.q3), aes(x=dist2017.q3)) +
  geom_density() +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  ggtitle('Recalibrated NPS Distribution - 2017 Q3')

mean(dist2017.q3)

# Calibrate Q2 ------------------------------------------------------------

nicheN <-
  breakdown[group=='niche' & type=='Q4 2017',proportion] * nrow(nps.q3)
otherN <-
  breakdown[group=='other' & type=='Q4 2017',proportion] * nrow(nps.q3)

dist2017.q2 <- c()
for (i in 1:1000) {
  npsq2Sample <-
    nps %>%
    filter(type=='Q2 2017' & group=='niche') %>%
    sample_n(size = nicheN,replace = T) %>%
    bind_rows(
      nps %>%
        filter(type=='Q2 2017' & group=='other') %>%
        sample_n(size = otherN,replace = T)
    )

  x <- getNPS(npsq2Sample$nps, 11)
  dist2017.q2 <- c(x,dist2017.q2)
}
ggplot(data.frame(dist2017.q2), aes(x=dist2017.q2)) +
  geom_density() +
  scale_x_continuous(breaks=pretty_breaks(10)) +
  ggtitle('Recalibrated NPS Distribution - 2017 Q2')

mean(dist2017.q2)