source('config/init.R')
library(glue)
library(scales)
first=F

q <-
  "SELECT
    l.date + 14 as date,
    SUM(l.calls + l.other) as extra_rbs
  FROM legacy_reportbacks l
  GROUP BY l.date"

aster <- runQuery(q)

q <-
  "
  SELECT
    dist.date,
    sum(dist.rbs) as rbs
  FROM
    (SELECT DISTINCT
        c.northstar_id,
        COALESCE(c.campaign_run_id::VARCHAR, c.campaign_id) as campaign_id,
        c.signup_id,
        c.post_class,
        c.reportback_volume AS rbs,
        c.post_attribution_date::date AS date
    FROM public.campaign_activity c
    WHERE c.post_attribution_date IS NOT NULL
    AND c.post_attribution_date >= '2014-01-01'
    AND c.post_status IN
      ('accepted','pending','register-OVR','register-form','confirmed')) dist
  GROUP BY dist.date"

qres <-
  runQuery(q)  %>%
  left_join(aster) %>%
  mutate(
    reportbacks =
      ifelse(!is.na(extra_rbs), rbs + extra_rbs, rbs)
  )

addRows <-
  tibble(
    date = seq.Date(max(qres$date)+1, as.Date('2019-12-31'), 1)
  )

rbs <-
  qres %>%
  bind_rows(addRows) %>%
  mutate(
    year = year(date),
    month = month(date),
    quarter = quarter(date),
    dayOfYear = yday(date)
  )

if (first==T) {

  rbMod <- lm(
    reportbacks ~ year + dayOfYear + year*dayOfYear,
    data=filter(rbs, !is.na(reportbacks))
  )

  save(rbMod, file='Data/reportbackForecast2019Model.RData')

} else {

  load('Data/reportbackForecast2019Model.RData')

}

rbs$expectRBs <- round(predict(rbMod, rbs, type='response'))

rbs %<>%
  mutate(
    runningTotal = ifelse(!is.na(reportbacks), cumsum(reportbacks), NA),
    expectRunTotal = cumsum(expectRBs)
  )

# Year over Year ----------------------------------------------------------

yoy <-
  rbs %>%
  group_by(year) %>%
  filter(year >= 2015) %>%
  mutate(
    yearRunningTotal = cumsum(reportbacks),
    expectedYearRunningTotal = cumsum(expectRBs)
  ) %>%
  filter(year(date)!=2020)

endVal <- max(yoy$expectedYearRunningTotal)
ticks <- c(endVal,seq(0,275000,25000))

p <-
  ggplot(yoy, aes(x=dayOfYear, y=yearRunningTotal, group=year)) +
  geom_line(aes(color=as.factor(year)), size=.5) +
  geom_line(aes(y=expectedYearRunningTotal,color=as.factor(year)),
            linetype='dotted', size=.75) +
  labs(x='Day of Year', y='Reportbacks',
       title='Reportbacks Over Time Per Year', colour='Year') +
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13)) +
  geom_segment(linetype='dotted',size=.25,x=-15,xend=365,y=endVal, yend=endVal) +
  scale_x_continuous(breaks=c(seq(0,340,20),365),limits=c(0,365)) +
  scale_y_continuous(breaks=ticks)
