source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

q <- "
SELECT
  date(submission_created_at) AS date,
  count(*) as reportbacks
FROM quasar.campaign_activity c
WHERE c.post_id <> -1
GROUP BY date(submission_created_at)
"

qres <- runQuery(q, which='mysql')

rbs <-
  qres %>% 
  mutate(
    date = as.Date(date),
    year = year(date)
    ) %>% 
  group_by(year) %>% 
  mutate(
    cumSumYear = cumsum(reportbacks),
    dayOfYear = yday(date)
  )

rbMod <- lm(
  cumSumYear ~ year + dayOfYear + year*dayOfYear,
  data=rbs
  )

maxDOY <-
  rbs %>% filter(year==2017) %>% 
  summarise(max=max(dayOfYear)) %>% 
  select(max) %>% 
  as.numeric()

addRows <-
  expand.grid(
    year = c(2018, 2019, 2020),
    dayOfYear = seq(1, 365, 1)
  ) %>% 
  as.tibble() %>% 
  bind_rows(
    data.frame(
      year = 2017,
      dayOfYear = seq(maxDOY,365,1)
    )
  ) %>% 
  arrange(year, dayOfYear)

rbs %<>%
  bind_rows(addRows)

rbs$expectedRBs <- round(predict(rbMod, rbs, type='response'))

q1DOY <- yday(as.Date('2017-03-31'))

EOYVals <- 
  rbs %>% 
  filter(dayOfYear==365 & year >= 2017) %>% data.table()

q1Vals <- 
  rbs %>% 
  filter(dayOfYear==q1DOY & year >= 2017) %>% data.table()

p <-
  ggplot(rbs, aes(x=dayOfYear, y=cumSumYear, group=year)) + 
  geom_line(aes(color=as.factor(year))) +
  geom_line(aes(y=expectedRBs, color=as.factor(year)), linetype='dotdash', size=.5) + 
  labs(x='Day of Year', y='Reportbacks', title='Reportbacks Over Time Per Year', colour='Year') + 
  theme(plot.title=element_text(hjust=0.5)) +
  geom_vline(aes(xintercept=q1DOY), linetype='dotdash') +
  geom_text(aes(x=q1DOY+5, label="End of Q1", y=250000, angle=90), size=3.5) + 
  scale_x_continuous(breaks=pretty_breaks(20)) +
  scale_y_continuous(breaks=c(EOYVals$expectedRBs, q1Vals$expectedRB, seq(100000, 300000, 50000))) 
 
for (i in 2017:2020) {
  p <- p + geom_segment(
    x=-15, 
    xend=365,
    y=EOYVals[year==i,expectedRBs],
    yend=EOYVals[year==i,expectedRBs], 
    linetype='dotted', size=.25
    )
  
  p <- p + geom_segment(
    x=-15, 
    xend=q1DOY,
    y=q1Vals[year==i,expectedRBs],
    yend=q1Vals[year==i,expectedRBs], 
    linetype='dotted', size=.25
  )
} 
