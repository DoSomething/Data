source('config/init.R')
library(glue)
library(scales)
first=F

q <-
  "SELECT
    SUBSTRING(m.timestamp::VARCHAR, 1, 7) as month_year,
    COUNT(DISTINCT m.northstar_id) as mams
  FROM member_event_log m
  WHERE m.timestamp >= '2015-01-01'
  AND m.action_type <> 'bertly_link_preview'
  AND m.source <> 'niche'
  GROUP BY SUBSTRING(m.timestamp::VARCHAR, 1, 7)"

qres <- runQuery(q)

mam <-
  qres %>%
  mutate(
    monthSeq = as.numeric(as.factor(month_year))
  ) %>%
  filter(monthSeq > 3)

if (first==T) {

  mamMod <- lm(mams ~ monthSeq, mam)
  save(mamMod, file='Data/reportbackForecast2019Model.RData')

} else {

  load('Data/reportbackForecast2019Model.RData')

}

datePads <-
  paste0(
    '2019-',
    ifelse(
      nchar(seq(1,12,1))<2,
      paste0('0',seq(1,12,1)),
      seq(1,12,1)
    )
  )

addRows <-
  tibble(
    monthSeq = seq(max(mam$monthSeq)+1, max(mam$monthSeq)+13),
    month_year = c('2018-12', datePads)
  )

mam %<>%
  bind_rows(addRows)

mam$expectMAM <- predict(mamMod, mam, type='response')

ticks <- c(seq(50000,300000,25000),max(mam$expectMAM))

ggplot(mam, aes(x=monthSeq, y=expectMAM)) +
  geom_line(linetype='dotdash') +
  geom_line(aes(y=mams)) +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq)+1,
    y=max(mam$expectMAM), yend=max(mam$expectMAM)
  ) +
  labs(x='',y='MAMs') +
  scale_x_continuous(breaks = mam$monthSeq, labels = mam$month_year) +
  scale_y_continuous(breaks = ticks, limits = c(50000,250000)) +
  theme(axis.text.x = element_text(angle=90,hjust=1))
