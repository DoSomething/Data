source('config/init.R')
source('config/mySQLConfig.R')
library(data.table)
library(scales)
library(googlesheets)
first=F

# Get 2018 Forecasts ------------------------------------------------------

rbSheetKey <- 
  gs_ls() %>% 
  filter(grepl('CJS',sheet_title)) %>% 
  select(sheet_key) %>% 
  as.character()

rbWB <- 
  gs_key(rbSheetKey)

sheetNames <- 
  gs_key(rbSheetKey) %>% 
  gs_ws_ls() 

rbSheet <-  rbWB %>% gs_read(sheetNames[3])

procSheet <- 
  rbSheet %>% 
  select(Month, Campaign, Source, Traffic, `Total Reportbacks`) %>% 
  do(na.locf(.)) %>% 
  rename(Reportbacks = `Total Reportbacks`) %>% 
  filter(grepl('DS.TurboVote.org',Campaign)) %>% 
  mutate(
    Traffic = as.numeric(gsub(',','',Traffic)),
    Reportbacks = as.numeric(gsub(',','',Reportbacks))
  )

rb2018Addition <- (sum(procSheet$Reportbacks) * 4) / 365

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
    date = seq.Date(max(qres$date)+1, as.Date('2020-12-31'), 1)
  )

rbs <-
  qres %>% 
  bind_rows(addRows) %>% 
  mutate(
    year = year(date),
    dayOfYear = yday(date)
  )

if (first==T) {
  
  rbMod <- lm(
    reportbacks ~ year + dayOfYear + year*dayOfYear,
    data=filter(rbs, !is.na(reportbacks))
  )
  
} else {
  load('Data/reportbackForecast2018Model.RData')
}

rbs$expectRBs <- round(predict(rbMod, rbs, type='response'))

rbs %<>%
  mutate(
    expectRBs = ifelse( (date >= '2018-01-01' & date < '2018-11-07') | 
                          (date >= '2020-01-01' & date < '2020-11-07'), 
                        round(expectRBs), expectRBs),
    # expectRBs_add = ifelse( (date >= '2018-01-01' & date < '2018-11-07') | 
    #                     (date >= '2020-01-01' & date < '2020-11-07'), 
    #                    round(expectRBs + rb2018Addition), expectRBs),
    runningTotal = ifelse(!is.na(reportbacks), cumsum(reportbacks), NA),
    expectRunTotal = cumsum(expectRBs)#,
    # expectRunTotal_all = cumsum(expectRBs_add)
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