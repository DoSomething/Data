source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

addRBs <- 
  read_csv('Data/additionalReportbacks2017.csv') %>% 
  mutate(
    date = as.Date(date, '%m/%d/%y'),
    additionalRBs = phone_calls + social_shares + voter_reg
    ) %>% 
  select(date, additionalRBs)

q <- "
SELECT
date(submission_created_at) AS date,
count(*) as reportbacks_looker
FROM quasar.campaign_activity c
WHERE c.post_id <> -1
GROUP BY date(submission_created_at)
"

qres <- 
  runQuery(q, which='mysql') %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(addRBs) %>% 
  mutate(
    reportbacks = ifelse(!is.na(additionalRBs), 
                         reportbacks_looker + additionalRBs, reportbacks_looker)
  )

addRows <- 
  tibble(
    date = seq.Date(max(qres$date)+1, as.Date('2020-12-31'), 1)
  )

rbs <-
  qres %>% 
  bind_rows(addRows) %>% 
  mutate(
    year = year(date),
    dayOfYear = yday(date)
  )

rbMod <- lm(
  reportbacks ~ year + dayOfYear + year*dayOfYear,
  data=filter(rbs, !is.na(reportbacks))
)

rbs$expectRBs <- round(predict(rbMod, rbs, type='response'))

rbs %<>%
  mutate(
    runningTotal = ifelse(!is.na(reportbacks), cumsum(reportbacks), NA),
    expectRunTotal = cumsum(expectRBs)
  )

q1s <- as.Date(c('2018-03-31','2019-03-31','2020-03-31'))
eoys <- as.Date(c('2018-01-01','2019-01-01','2020-01-01','2020-12-31'))

datesOfInterest <-
  rbs %>% 
  filter(date %in% c(q1s, eoys)) %>% 
  data.table()

p <- 
  ggplot(rbs, aes(x=date)) + 
  geom_line(aes(y=expectRunTotal)) + 
  geom_line(aes(y=runningTotal), color='red') + 
  geom_vline(xintercept = q1s, linetype='dotted') + 
  scale_x_date(breaks=pretty_breaks(20)) + 
  scale_y_continuous(breaks=c(datesOfInterest$expectRunTotal, seq(0, 1100000, 125000))) + 
  labs(x='Day of Year', y='Reportbacks', title='Reportback Expectations Over Time') + 
  theme(plot.title=element_text(hjust=0.5))
  
for (i in 1:length(datesOfInterest$date)) {
  p <- p + 
    geom_segment(
      x=-15, 
      xend=datesOfInterest[i,date],
      y=datesOfInterest[i,expectRunTotal],
      yend=datesOfInterest[i,expectRunTotal], 
      linetype='dotted', size=.25
    )
}

# Year over Year ----------------------------------------------------------

yoy <- 
  rbs %>% 
  group_by(year) %>% 
  mutate(
    yearRunningTotal = cumsum(reportbacks),
    expectedYearRunningTotal = cumsum(expectRBs)
  )

q1DOY <- yday(as.Date('2017-03-31'))

EOYVals <- 
  yoy %>% 
  filter(dayOfYear==365 & year >= 2017) %>% data.table()

q1Vals <- 
  yoy %>% 
  filter(dayOfYear==q1DOY & year >= 2017) %>% data.table()

ticks <- c(EOYVals$expectedYearRunningTotal, 
           q1Vals$expectedYearRunningTotal, 
           0,25000,75000,100000,125000,250000)

p <-
  ggplot(yoy, aes(x=dayOfYear, y=yearRunningTotal, group=year)) + 
  geom_line(aes(color=as.factor(year)), size=.5) +
  geom_line(aes(y=expectedYearRunningTotal, color=as.factor(year)), 
            linetype='dashed', size=.75) + 
  labs(x='Day of Year', y='Reportbacks', 
       title='Reportbacks Over Time Per Year', colour='Year') + 
  theme(plot.title=element_text(hjust=0.5)) +
  geom_vline(aes(xintercept=q1DOY), linetype='dotdash') +
  geom_text(aes(x=q1DOY+5, label="End of Q1", y=200000, angle=90), size=3.5) + 
  scale_x_continuous(breaks=pretty_breaks(20)) +
  scale_y_continuous(breaks=ticks)# + ylim(0,250000)

for (i in 2017:2020) {
  p <- p + geom_segment(
    x=-15, 
    xend=365,
    y=EOYVals[year==i,expectedYearRunningTotal],
    yend=EOYVals[year==i,expectedYearRunningTotal], 
    linetype='dotted', size=.25
  )
  
  p <- p + geom_segment(
    x=-15, 
    xend=q1DOY,
    y=q1Vals[year==i,expectedYearRunningTotal],
    yend=q1Vals[year==i,expectedYearRunningTotal], 
    linetype='dotted', size=.25
  )
} 