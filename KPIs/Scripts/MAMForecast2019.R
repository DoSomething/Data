source('config/init.R')
library(glue)
library(scales)
library(grid)
library(gridExtra)
first=T

q <-
  "SELECT
    SUBSTRING(m.timestamp::VARCHAR, 1, 7) as month_year,
    COUNT(
      DISTINCT
        CASE WHEN m.action_type <> 'bertly_link_preview' THEN m.northstar_id
        ELSE NULL END
    ) as mams,
    COUNT(DISTINCT m.northstar_id) as mams_old
  FROM member_event_log m
  WHERE m.timestamp >= '2015-01-01'
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
  mamMod.old <- lm(mams_old ~ monthSeq, mam)
  save(mamMod, mamMod.old, file='Data/MAMForecast2019Model.RData')

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
    )
  )

addRows <-
  tibble(
    monthSeq = seq(max(mam$monthSeq)+1, max(mam$monthSeq)+13),
    month_year = c('2018-12', datePads)
  )

mam %<>%
  bind_rows(addRows) %>%
  mutate(year=as.numeric(substr(month_year,1,4)))

mam$expectMAM = predict(mamMod, mam, type='response')
mam$expectMAM.old = predict(mamMod.old, mam, type='response')

for (i in 1:nrow(mam)) {

  mam[i,'expectMAM.alt'] <-
    ifelse(
      !is.na(mam[i,'mams']),
      mam[i,'mams'],
      mam[i-1,'expectMAM.alt']+mam[i-1,'expectMAM.alt']*.02
    )

  mam[i,'expectMAM.altOLD'] <-
    ifelse(
      !is.na(mam[i,'mams_old']),
      mam[i,'mams_old'],
      mam[i-1,'expectMAM.altOLD']+mam[i-1,'expectMAM.altOLD']*.02
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

ticksOld <-
  round(c(
    seq(50000,325000,25000),
    max(mam$expectMAM.altOLD),max(mam$expectMAM.old)
  ))

pold <-
  ggplot(mam, aes(x=monthSeq, y=expectMAM.old)) +
  geom_line(aes(y=expectMAM.altOLD), linetype='dotdash', color='blue') +
  geom_line(aes(y=mams_old)) +
  geom_line(linetype='dotdash') +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq),
    y=max(mam$expectMAM.old), yend=max(mam$expectMAM.old)
  ) +
  geom_segment(
    linetype='dotted',size=.25,x=-15,xend=max(mam$monthSeq),
    y=max(mam$expectMAM.altOLD), yend=max(mam$expectMAM.altOLD)
  ) +
  labs(x='', y='', title='With Previews') +
  scale_x_continuous(breaks = mam$monthSeq, labels = mam$month_year) +
  scale_y_continuous(breaks = ticksOld, limits = c(50000,325000)) +
  theme(
    plot.title=element_text(hjust=0.5),
    axis.text.x = element_text(angle=90),
    axis.text.y = element_text(face="bold", size=13)
  )

grid.arrange(pnew, pold, ncol=2, top=textGrob('MAMs Over Time',gp=gpar(fontsize=15)))

actualNoPreview <-
  mam %>% filter(year==2018) %$% mams %>% mean(.,na.rm=T)
actualWithPreview <-
  mam %>% filter(year==2018) %$% mams_old %>% mean(., na.rm=T)

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

