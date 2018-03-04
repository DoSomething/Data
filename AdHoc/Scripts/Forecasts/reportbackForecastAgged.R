source('config/init.R')
source('config/mySQLConfig.R')
library(data.table)
library(scales)
library(googlesheets)
first=T

# Get 2018 Forecasts ------------------------------------------------------

# rbSheetKey <- 
#   gs_ls() %>% 
#   filter(grepl('CJS',sheet_title)) %>% 
#   select(sheet_key) %>% 
#   as.character()
# 
# rbWB <- 
#   gs_key(rbSheetKey)
# 
# sheetNames <- 
#   gs_key(rbSheetKey) %>% 
#   gs_ws_ls() 
# 
# rbSheet <-  rbWB %>% gs_read(sheetNames[3])
# 
# procSheet <- 
#   rbSheet %>% 
#   select(Month, Campaign, Source, Traffic, `Total Reportbacks`) %>% 
#   do(na.locf(.)) %>% 
#   rename(Reportbacks = `Total Reportbacks`) %>% 
#   filter(grepl('DS.TurboVote.org',Campaign)) %>% 
#   mutate(
#     Traffic = as.numeric(gsub(',','',Traffic)),
#     Reportbacks = as.numeric(gsub(',','',Reportbacks))
#   )
# 
# rb2018Addition <- (sum(procSheet$Reportbacks) * 4) / 365

# Get Additional Reportbacks ----------------------------------------------

source('Scripts/rbAsteriskToQuasar.R')

addRBs <- 
  reportbacks_asterisk %>% 
  group_by(date) %>% 
  summarise(
    rbs = sum(rbs)
  )

# Get Regular Reportbacks -------------------------------------------------

q <- "
SELECT
  ca.date,
  SUM(ca.reportback) AS reportbacks_looker
FROM
  (SELECT
    c.signup_id,
    MAX(date(submission_created_at)) AS date,
    MAX(CASE WHEN c.post_id <> -1 THEN 1 ELSE 0 END) AS reportback
  FROM quasar.campaign_activity c
  WHERE c.post_id <> -1
  GROUP BY c.signup_id
  ) ca
GROUP BY ca.date
"

qres <- 
  runQuery(q, which='mysql') %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(addRBs) %>% 
  mutate(
    reportbacks = 
      ifelse(!is.na(rbs), reportbacks_looker + rbs, reportbacks_looker)
  )

addRows <- 
  tibble(
    date = seq.Date(max(qres$date)+1, as.Date('2019-01-01'), 1)
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

rbMonthMakeup <- 
  rbs %>% 
  group_by(year, month) %>% 
  summarise(t=sum(reportbacks, na.rm=T)) %>% 
  mutate(p=t/sum(t)) %>% 
  filter(!year %in% c(2014,2018,2019)) %>% 
  group_by(month) %>% 
  mutate(
    monthMean = mean(p)
  ) %>% ungroup() %>% 
  mutate(
    quarter = 
      case_when(month %in% 1:3 ~ 1,
                month %in% 4:6 ~ 2, 
                month %in% 7:9 ~ 3, 
                TRUE ~ 4)
  ) %>% 
  group_by(quarter) %>% 
  mutate(
    quarterMean = mean(p)
  )

ggplot(rbMonthMakeup, aes(x=as.factor(month), y=p, fill=as.factor(year))) +
  geom_bar(stat='identity', position='dodge') +
  geom_line(aes(y=monthMean, group=year), linetype='dotted', size=.3) +
  geom_point(aes(y=monthMean, group=year)) +
  geom_point(aes(y=quarterMean, group=year), shape=3) + 
  geom_hline(yintercept = 1/12) + 
  annotate("text", x=1/12, y=1/12, vjust = -1, hjust=-.1, size=2.2, 
           label = "Even Distribution Mark") +
  scale_fill_brewer(palette='Set2') + 
  scale_y_continuous(breaks=pretty_breaks(10)) + 
  guides(fill=guide_legend(title="Year")) +
  labs(title='Reportback Distribution Within Year', x='Month', y='Proportion') +
  theme(plot.title=element_text(hjust=.5))

if (first==T) {
  
  rbMod <- lm(
    reportbacks ~ year + dayOfYear + year*dayOfYear,
    data=filter(rbs, !is.na(reportbacks))
  )  
  rbModMon <- lm(
    reportbacks ~ year + dayOfYear + year*dayOfYear + as.factor(month),
    data=filter(rbs, !is.na(reportbacks))
  )
  
} else {
  load('Data/reportbackForecast2018Model.RData')
}

rbs$expectRBs <- round(predict(rbMod, rbs, type='response'))
rbs$expectRBs.Mon <- round(predict(rbModMon, rbs, type='response'))

rbs %<>%
  mutate(
    expectRBs = ifelse( (date >= '2018-01-01' & date < '2018-11-07') | 
                          (date >= '2020-01-01' & date < '2020-11-07'), 
                        round(expectRBs), expectRBs),
    expectRBs.Mon = ifelse( (date >= '2018-01-01' & date < '2018-11-07') | 
                          (date >= '2020-01-01' & date < '2020-11-07'), 
                        round(expectRBs.Mon), expectRBs.Mon),
    runningTotal = ifelse(!is.na(reportbacks), cumsum(reportbacks), NA),
    expectRunTotal = cumsum(expectRBs),
    expectRunTotal.Mon = cumsum(expectRBs.Mon)
  )

q1s <- as.Date(c('2018-03-31','2019-03-31','2020-03-31'))
q2018 <- as.Date(c('2018-03-31','2018-06-30','2018-09-31','2018-12-31'))
eoys <- as.Date(c('2018-01-01','2019-01-01','2020-01-01','2020-12-31'))

datesOfInterest <-
  rbs %>% 
  filter(date %in% c(q2018)) %>% 
  data.table()

# Year over Year ----------------------------------------------------------

yoy <- 
  rbs %>% 
  group_by(year) %>% 
  mutate(
    yearRunningTotal = cumsum(reportbacks),
    expectedYearRunningTotal = cumsum(expectRBs),
    expectedYearRunningTotal.Mon = cumsum(expectRBs.Mon)
  ) %>% 
  filter(year(date)!=2019)

DOYs <- c(yday(as.Date('2017-03-31')),
          yday(as.Date('2017-06-30')),
          yday(as.Date('2017-09-30')),
          yday(as.Date('2017-12-31')))

qVals <- 
  yoy %>% 
  filter(dayOfYear %in% DOYs & year > 2017) %>% data.table()

ticks <- c(qVals$expectedYearRunningTotal, 
           0,25000,50000,75000,100000,150000,175000)

p <-
  ggplot(yoy, aes(x=dayOfYear, y=yearRunningTotal, group=year)) + 
  geom_line(aes(color=as.factor(year)), size=.5) +
  geom_line(aes(y=expectedYearRunningTotal), 
            linetype='dotted', size=.75) + 
  geom_line(aes(y=expectedYearRunningTotal.Mon, color=as.factor(year)), 
            linetype='dashed', size=.75) + 
  labs(x='Day of Year', y='Reportbacks', 
       title='Reportbacks Over Time Per Year', colour='Year') + 
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13)) +
  geom_vline(aes(xintercept=DOYs[1]), linetype='dotdash') +
  geom_text(aes(x=DOYs[1]+5, label="End of Q1", y=175000, angle=90), size=3.5) + 
  scale_x_continuous(breaks=pretty_breaks(20),limits=c(0,365)) +
  scale_y_continuous(breaks=ticks) 

for (i in 1:length(qVals)) {
  p <- p + geom_segment(
    x=-15, 
    xend=DOYs[i],
    y=qVals[i,expectedYearRunningTotal],
    yend=qVals[i,expectedYearRunningTotal], 
    linetype='dotted', size=.25
  )
} 

# All in One --------------------------------------------------------------

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
