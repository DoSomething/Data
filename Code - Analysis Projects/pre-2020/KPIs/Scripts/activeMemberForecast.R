source('config/init.R')
source('config/mySQLConfig.R')
library(reshape2)
library(data.table)
library(scales)

# q <- "
# SELECT DISTINCT
#   baddies.northstar_id
# FROM (
#   SELECT
#     u.northstar_id,
#     count(DISTINCT ca.signup_id) AS total_signups,
#     sum(case when post_id <> -1 then 1 else 0 end) as total_rbs,
#     max(case when greatest(u.last_accessed, u.last_logged_in) > u.created_at THEN 1 ELSE 0 END) as activated
#   FROM quasar.users u
#   LEFT JOIN quasar.campaign_activity ca
#   ON u.northstar_id=ca.northstar_id
#   WHERE u.source='niche'
#   GROUP BY u.northstar_id) baddies
# WHERE baddies.total_signups <= 1
# AND baddies.total_rbs < 1
# AND baddies.activated = 0"
#
# badNiche <- runQuery(q, 'mysql')
#
# q <- sprintf("
# SELECT
#   m.northstar_id AS nsid,
#   m.action_type AS type,
#   m.event_id,
#   m.timestamp AS ts
# FROM quasar.member_event_log m
# WHERE m.timestamp >= '2014-01-01'
# AND m.timestamp <= %s
# ", sQuote(Sys.Date()))
#
# qres <- runQuery(q, 'mysql') %>% data.table()
#
# # mel <-
# #   read_csv('Data/entire_MEL_11_29_17.csv') %>%
# #   setNames(c('nsid','type','event_id','ts')) %>%
# #   bind_rows(
# #     read_csv('Data/entire_MEL_11_30_17_niche.csv') %>%
# #       setNames(c('nsid','type','event_id','ts'))
# #   ) %>%
# #   data.table()
#
# mel <-
#   qres[
#   order(nsid, ts)
#   ][
#     ts > as.Date('2014-01-01') &
#       ts <= Sys.Date() &
#       !is.na(nsid) & nsid != 'NULL' &
#       !duplicated(event_id)
#   ][
#   ,
#   ':='(
#     actionSeq = 1:.N
#   )
#   ,
#   by = nsid
# ][
#   ,
#   first_action := ifelse(actionSeq==1, 1, 0)
# ]
#
# melMon <-
#   mel[,
#     yearMonth := substr(ts, 1, 7)
#   ][!duplicated(paste0(nsid, yearMonth))
#     ][,
#     .(
#       distinctNS = length(unique(nsid))
#     ),
#     by=.(yearMonth, first_action)
#   ]


melMon <-
  read_csv('Data/MAM_Forecast_Breakdown_03022018.csv') %>%
  setNames(c('yearMonth','return_user','distinctNS')) %>%
  filter(yearMonth != '2017-04') %>%
  mutate(
    new_user = ifelse(return_user=='Yes', 0, 1)
  )

melMon <- dcast(melMon, yearMonth ~ new_user, value.var = 'distinctNS')
names(melMon) <- c('yearMonth','Repeat', 'New')

melMon %<>%
  mutate(
    year=as.numeric(substr(yearMonth,1,4)),
    month=as.numeric(substr(yearMonth,6,7))
  )

melMonCast <-
  melMon %>%
  mutate(
    Repeat = ifelse(is.na(Repeat), 0, Repeat),
    total = Repeat + New,
    dateYearMonth = as.Date(paste0(yearMonth, '-01'))
    ) %>%
  filter(dateYearMonth >= '2016-05-01' & yearMonth != '2018-03') %>%
  mutate(yearMonth.n = as.numeric(as.factor(yearMonth)))

newMod <- lm(New ~ yearMonth.n + I(month%in%c(1,2,3)) + I(month%in%c(4,5,6)) +
               I(month%in%c(7,8,9)) + I(month%in%c(10,11,12)),melMonCast)
repeatMod <- lm(Repeat ~ year + I(month%in%c(1,2,3)) + I(month%in%c(4,5,6)) +
                  I(month%in%c(7,8,9)) + I(month%in%c(10,11,12)), melMonCast)
totalMod <- lm(total ~ year + I(month%in%c(1,2,3)) + I(month%in%c(4,5,6)) +
                 I(month%in%c(7,8,9)) + I(month%in%c(10,11,12)), melMonCast)

addRows <-
  expand.grid(
    year = c(2018, 2019),
    month = seq(1,12,1)
  ) %>%
  mutate(
    dateYearMonth = as.Date(paste0(year,'-' ,month,'-01'), '%Y-%m-%d'),
    yearMonth = substr(dateYearMonth, 1, 7)
  ) %>%
  filter(dateYearMonth > max(melMonCast$dateYearMonth) &
           dateYearMonth <= '2018-12-01')

melMonCast %<>%
  bind_rows(addRows) %>%
  arrange(yearMonth) %>%
  mutate(yearMonth.n = as.numeric(as.factor(yearMonth)))

melMonCast$eNew <- round(predict(newMod, melMonCast, type='response'))
melMonCast$eRepeat <- round(predict(repeatMod, melMonCast, type='response'))
melMonCast$eTotal <- round(predict(totalMod, melMonCast, type='response'))

# rowNeeded <-
#   melMonCast %>%
#   filter(!is.na(total)) %>%
#   filter(yearMonth.n==max(yearMonth.n)) %>%
#   select(yearMonth.n) %>% as.numeric()

melMonCast %<>%
  mutate(
    eNew = ifelse(is.na(New), NA, eNew)
  ) %>% data.table()

fillMiss <- function(dat) {
  for (i in 1:nrow(dat)) {
    if(is.na(dat[i,eNew])==T) {
      lastVal <- dat[i-1,eNew]
      dat[i,eNew := lastVal + eRepeat-dat[i-1,eRepeat]]
    }
  }
  return(dat)
}
melMonCast <- fillMiss(melMonCast)
melMonCast %<>% mutate(eTotal = eNew + eRepeat)

NR <-
  melMonCast %>%
  select(dateYearMonth, Repeat, New, eNew, eRepeat) %>%
  melt(id.vars='dateYearMonth') %>%
  mutate(
    Group = ifelse(grepl('Repeat', variable), 'Repeat', 'New'),
    Type = ifelse(substr(variable, 1, 1)=='e', 'Predicted', 'Actual')
  ) %>%
  as.tibble() %>%
  select(-variable) %>%
  dcast(dateYearMonth + Group ~ Type, value.var='value')


eoys <- as.Date(c('2018-04-01','2018-07-01','2018-10-01','2019-01-01'))

eoyVals <-
  melMonCast %>%
  filter(dateYearMonth %in% eoys) %>%
  data.table()

ticks <- c(round(eoyVals$eTotal),
           seq(0,150000, 25000),
           c(200000,250000),
           seq(350000, 450000, 50000))

p <-
  ggplot() +
  geom_line(data=NR, aes(y=Actual, x=dateYearMonth, color=Group)) +
  geom_line(data=NR, aes(y=Predicted, x=dateYearMonth, color=Group), linetype='dotdash') +
  geom_line(data=melMonCast, aes(x=dateYearMonth, y=round(eTotal)), linetype='dotdash') +
  geom_line(data=melMonCast, aes(x=dateYearMonth, y=total)) +
  labs(x='Date', y='Monthly Active Members',
       title='Monthly Active Members Over Time - Including Niche') +
  theme(plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13)) +
  scale_x_date(breaks=pretty_breaks(17),
               limits=c(min(NR$dateYearMonth), max(NR$dateYearMonth)),
               expand = c(0,0)) +
  scale_y_continuous(breaks=ticks)

for (i in 1:length(eoyVals$dateYearMonth)) {
  p <- p +
    annotate('segment',
      x=as.Date('2016-06-01'),
      xend=eoyVals[i,dateYearMonth],
      y=eoyVals[i,round(eTotal)],
      yend=eoyVals[i,round(eTotal)],
      linetype='dotted', size=.5
    )
}

# Daily Actions -----------------------------------------------------------

da <-
  read_csv('Data/daily_active_members.csv') %>%
  setNames(c('date','users')) %>%
  filter(!is.na(users) & date != '2017-04-06')

ggplot(da, aes(x=date, y=users)) +
  geom_point() + geom_smooth() +
  scale_x_date(breaks=pretty_breaks(10))
