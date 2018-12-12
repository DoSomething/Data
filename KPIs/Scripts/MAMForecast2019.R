source('config/init.R')
library(glue)
library(scales)
library(grid)
library(gridExtra)
first=T

q <-
  "SELECT
    m.month_year,
    COUNT(DISTINCT m.northstar_id) as mams,
    COUNT(
      DISTINCT
        CASE WHEN m.type = 'New' THEN m.northstar_id ELSE NULL END
    ) as new_mams,
    COUNT(
      DISTINCT
        CASE WHEN m.type = 'Renewal' THEN m.northstar_id ELSE NULL END
        ) as renewal_mams
  FROM
    (SELECT
      mt1.northstar_id,
      DATE_TRUNC('month', mt1.timestamp) + interval '4h' as month_year,
      CASE
        WHEN MAX(mt1.first_action) = 1 THEN 'New'
        ELSE 'Renewal' END AS type
    FROM
      (SELECT
        mt.northstar_id,
        mt.timestamp,
        mt.action_type,
        CASE WHEN
          (min(mt.timestamp) OVER (PARTITION BY mt.northstar_id)) = mt.timestamp
          THEN 1
          ELSE 0 END AS first_action
      FROM member_event_log mt
      WHERE mt.source <> 'niche'
      AND mt.action_type <> 'bertly_link_preview') mt1
    GROUP BY
      mt1.northstar_id,
      DATE_TRUNC('month', mt1.timestamp) + interval '4h'
    ) m
  WHERE m.month_year >= '2015-01-01'
  GROUP BY m.month_year"

qres <- runQuery(q)

mam <-
  qres %>%
  mutate(
    monthSeq = as.numeric(as.factor(month_year))
  ) %>%
  filter(monthSeq > 3 & monthSeq < 48)

if (first==T) {

  mamMod <- lm(mams ~ monthSeq, mam)
  mamMod.new <- lm(new_mams ~ monthSeq, filter(mam, month_year>='2017-01-01'))
  mamMod.renewal <- lm(renewal_mams ~ monthSeq, filter(mam, month_year>='2017-01-01'))
  save(mamMod, mamMod.new, mamMod.renewal, file='Data/MAMForecast2019Model.RData')

} else {

  load('Data/MAMForecast2019Model.RData')

}

datePads <-
  paste0(
    '2019-',
    ifelse(
      nchar(seq(1,12,1))<2,
      paste0('0',seq(1,12,1)),
      seq(1,12,1)
    ),
    '-01'
  )

addRows <-
  tibble(
    monthSeq = seq(max(mam$monthSeq)+1, max(mam$monthSeq)+13),
    month_year = as.Date(c('2018-12-01', datePads))
  )

mam %<>%
  mutate(month_year = as.Date(month_year)) %>%
  bind_rows(addRows) %>%
  mutate(year=as.numeric(substr(month_year,1,4)))

mam$expectMAM = predict(mamMod, mam, type='response')
mam$expectMAM.new = predict(mamMod.new, mam, type='response')
mam$expectMAM.renewal = predict(mamMod.renewal, mam, type='response')

for (i in 1:nrow(mam)) {

  mam[i,'expectMAM.alt'] <-
    ifelse(
      !is.na(mam[i,'mams']),
      mam[i,'mams'],
      mam[i-1,'expectMAM.alt']+mam[i-1,'expectMAM.alt']*.02
    )

}

ticksNew <-
  round(c(
    seq(50000,225000,25000), seq(275000,325000,25000),
    max(mam$expectMAM),max(mam$expectMAM.alt)
  ))

pnew <-
  ggplot(mam, aes(x=monthSeq, y=mams)) +
  geom_line(aes(y=expectMAM.alt), linetype='dotdash', color='blue') +
  geom_line(aes(y=expectMAM), linetype='dotdash') +
  geom_line() +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq),
    y=max(mam$expectMAM), yend=max(mam$expectMAM)
  ) +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq),
    y=max(mam$expectMAM.alt), yend=max(mam$expectMAM.alt)
  ) +
  labs(x='', y='', title='No Previews') +
  scale_x_continuous(breaks = mam$monthSeq, labels = mam$month_year) +
  scale_y_continuous(breaks = ticksNew, limits = c(50000,325000)) +
  theme(
    plot.title=element_text(hjust=0.5),
    axis.text.x = element_text(angle=90),
    axis.text.y = element_text(face="bold", size=13)
    )

ticksNR <-  round(c(seq(0,200000,25000),max(mam$expectMAM.new),max(mam$expectMAM.renewal)))
ggplot(mam, aes(x=monthSeq)) +
  geom_line(aes(y=expectMAM.new), linetype='dotdash', color='blue') +
  geom_line(aes(y=expectMAM.renewal), linetype='dotdash', color='red') +
  geom_line(aes(y=new_mams), color='blue') +
  geom_line(aes(y=renewal_mams), color='red') +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq),
    y=max(mam$expectMAM.renewal), yend=max(mam$expectMAM.renewal)
  ) +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq),
    y=max(mam$expectMAM.new), yend=max(mam$expectMAM.new)
  ) +
  scale_x_continuous(breaks = mam$monthSeq, labels = mam$month_year) +
  scale_y_continuous(breaks = ticksNR, limits = c(0,200000)) +
  labs(x='', y='', title='New Renewal Breakout') +
  theme(
    plot.title=element_text(hjust=0.5),
    axis.text.x = element_text(angle=90),
    axis.text.y = element_text(face="bold", size=13)
  )

actualNoPreview <-
  mam %>% filter(year==2018) %$% mams %>% mean(.,na.rm=T)

forecastsMAM <-
  mam %>%
  filter(year>2018) %>%
  summarise(
    Line.WithPreviews = mean(expectMAM.old),
    Line.NoPreviews = mean(expectMAM),
    twoPct.WithPreviews = mean(expectMAM.altOLD),
    twoPct.NoPreviews = mean(expectMAM.alt)
  ) %>%
  gather(type, forecast) %>%
  mutate(
    Actual.2018 = ifelse(grepl('NoPrev', type), actualNoPreview, actualWithPreview),
    pctChange = pctChange(Actual.2018, forecast)
  ) %>%
  select(Type = type, Actual.2018, Forecast.2019 = forecast, pctChange) %>%
  mutate(
    Forecast.2019 = formatC(Forecast.2019, format="d", big.mark=","),
    Actual.2018 = formatC(Actual.2018, format="d", big.mark=",")
  )

