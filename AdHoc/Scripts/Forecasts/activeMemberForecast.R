source('config/init.R')
source('config/mySQLConfig.R')
library(scales)

q <- "
SELECT DISTINCT
  baddies.northstar_id
FROM (
  SELECT 
    u.northstar_id,
    count(DISTINCT ca.signup_id) AS total_signups,
    sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
    max(case when greatest(u.last_accessed, u.last_logged_in) > u.created_at THEN 1 ELSE 0 END) as activated
  FROM quasar.users u
  LEFT JOIN quasar.campaign_activity ca
  ON u.northstar_id=ca.northstar_id
  WHERE u.source='niche'
  GROUP BY u.northstar_id) baddies
WHERE baddies.total_signups <= 1 
AND baddies.total_rbs < 1
AND baddies.activated = 0"

badNiche <- runQuery(q, 'mysql')

mel <- 
  read_csv('Data/entire_MEL_11_29_17.csv') %>% 
  setNames(c('nsid','type','event_id','ts')) %>% 
  bind_rows(
    read_csv('Data/entire_MEL_11_30_17_niche.csv') %>%
      setNames(c('nsid','type','event_id','ts'))
  ) %>%
  data.table()

mel <- 
  mel[
  order(nsid, ts)
  ][
    ts > as.Date('2014-01-01') & ts <= Sys.Date() & !is.na(nsid) & nsid != 'NULL'
  ][
  ,
  ':='(
    actionSeq = 1:.N
  )
  ,
  by = nsid
][
  ,
  first_action := ifelse(actionSeq==1, 1, 0)
]

melMon <- 
  mel[,
    yearMonth := substr(ts, 1, 7)
  ][!duplicated(paste0(nsid, yearMonth))
    ][,
    .(
      distinctNS = length(unique(nsid))
    ),
    by=.(yearMonth, first_action)
  ]

melMon <- dcast(melMon, yearMonth ~ first_action, value.var = 'distinctNS')
names(melMon) <- c('yearMonth','Repeat', 'New')

melMon[,
  ':='(
    year=as.numeric(substr(yearMonth,1,4)),
    month=as.numeric(substr(yearMonth,6,7))
  )
]

melMonCast <-
  melMon %>% 
  as.tibble() %>% 
  mutate(
    Repeat = ifelse(is.na(Repeat), 0, Repeat),
    total = Repeat + New,
    dateYearMonth = as.Date(paste0(yearMonth, '-01'))
    ) %>% 
  filter(year >= 2015 & yearMonth != '2017-11') %>% 
  mutate(yearMonth.n = as.numeric(as.factor(yearMonth)))

newMod <- lm(New ~ yearMonth.n, melMonCast)
repeatMod <- lm(Repeat ~ yearMonth.n, melMonCast)
totalMod <- lm(total ~ yearMonth.n, melMonCast)

addRows <- 
  expand.grid(
    year = c(2018, 2019, 2020),
    month = seq(1,12,1)
  ) %>% 
  bind_rows(
    data.frame(year=c(2017, 2017), month=c(11,12))
  ) %>% 
  mutate(
    dateYearMonth = as.Date(paste0(year,'-' ,month,'-01'), '%Y-%m-%d'),
    yearMonth = substr(dateYearMonth, 1, 7)
  )

melMonCast %<>%
  bind_rows(addRows) %>% 
  arrange(year, month) %>% 
  mutate(yearMonth.n = as.numeric(as.factor(yearMonth)))

melMonCast$eNew <- round(predict(newMod, melMonCast, type='response'))
melMonCast$eRepeat <- round(predict(repeatMod, melMonCast, type='response'))
melMonCast$eTotal <- round(predict(totalMod, melMonCast, type='response'))

rowNeeded <- 
  melMonCast %>% 
  filter(!is.na(total)) %>% 
  filter(yearMonth.n==max(yearMonth.n)) %>% 
  select(yearMonth.n) %>% as.numeric()

melMonCast %<>%
  mutate(
    eNew = ifelse(
      is.na(New), 
      as.numeric(melMonCast[rowNeeded,'eNew']) + tidy(repeatMod)$estimate[2]*(yearMonth.n-rowNeeded), 
      eNew),
    eTotal = eNew + eRepeat
    )

NR <- 
  melMonCast %>% 
  select(dateYearMonth, Repeat, New, eNew, eRepeat) %>% 
  melt(id.vars='dateYearMonth') %>% 
  mutate(
    Group = ifelse(grepl('Repeat', variable), 'Repeat', 'New'),
    Type = ifelse(substr(variable, 1, 1)=='e', 'Predicted', 'Actual')
  ) %>% 
  dcast(dateYearMonth + Group ~ Type, value.var='value')


eoys <- as.Date(c('2018-01-01','2019-01-01','2020-01-01','2020-12-01'))

eoyVals <-
  melMonCast %>% 
  filter(dateYearMonth %in% eoys) %>% 
  data.table()

ticks <- c(round(eoyVals$eTotal), 275000,
           seq(0,150000, 25000), seq(200000, 325000, 25000))

p <-
  ggplot() + 
  geom_line(data=NR, aes(y=Actual, x=dateYearMonth, color=Group)) +
  geom_line(data=NR, aes(y=Predicted, x=dateYearMonth, color=Group), linetype='dotdash') +
  geom_line(data=melMonCast, aes(x=dateYearMonth, y=round(eTotal)), linetype='dotdash') +
  geom_line(data=melMonCast, aes(x=dateYearMonth, y=total)) +
  labs(x='Date', y='Monthyl Active Members', 
       title='Monthly Active Members Over Time - Including Niche') +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_x_date(breaks=pretty_breaks(17), 
               limits=c(as.Date('2015-01-01'), as.Date('2020-12-31')),
               expand = c(0,0)) +
  scale_y_continuous(breaks=ticks)

for (i in 1:length(eoyVals$dateYearMonth)) {
  p <- p + 
    annotate('segment',
      x=as.Date('2015-01-01'), 
      xend=eoyVals[i,dateYearMonth],
      y=eoyVals[i,round(eTotal)],
      yend=eoyVals[i,round(eTotal)], 
      linetype='dotted', size=.5
    )
}

api_create(ggplotly(p), filename = "test_plot")
