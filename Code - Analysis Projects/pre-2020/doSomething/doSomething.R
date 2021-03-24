library(data.table)
library(dplyr)
library(dtplyr)
library(lubridate)
library(ggplot2)
library(ggvis)
library(tidyr)
library(ggthemes)
options(scipen=50)
add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title") {
  add_axis(vis, "x", title = x_lab) %>% 
    add_axis("x", orient = "top", ticks = 0, title = title,
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0)
             ), ...)
}

# Data prep ---------------------------------------------------------------
df <- tbl_dt(read.csv('~/Downloads/Sohaib_the_all_inclusive member_event_log 2017-04-25T1100.csv'))

colnames(df) <- gsub('Member.Event.Log.', '', colnames(df))

df[,dateTime := as.POSIXct(Timestamp.Unix, origin="1970-01-01")]

##Events are being logged mmore than once
dups <- df[duplicated(df) | duplicated(df, fromLast = T)]
ds <- df[!duplicated(df)][order(Uid, dateTime)]

##Event id doesn't necessarily seem to uniquely identify an action, though perhpas there is some logic being applied
eventDebug <- ds[duplicated(Event.ID) | duplicated(Event.ID, fromLast = T)]


# Time Series questions ---------------------------------------------------

ds <- 
  ds %>%
    mutate(
      monthYear = paste0(year(dateTime),'-',month(dateTime)),
      weekYear = paste0(year(dateTime), '-', strftime(dateTime,format="%W")),
      Day = as.Date(dateTime),
      userMonth = paste0(Uid, '-', month(Day)),
      Month = month(dateTime)
  ) 

usersOverTime <- 
  ds %>%
  group_by(Day) %>%
  summarise(
    nUsers = length(unique(Uid)),
    nActions = n()
  ) %>%
  mutate(
    dayOfWeek = factor(weekdays(Day), levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')),
    weekend = ifelse(dayOfWeek %in% c('Saturday','Sunday'), T, F)
    )
col = colorRampPalette(c("blue","red"))
palette <- col(7)
usersOverTime.vis <- 
  ggvis(usersOverTime, ~Day, ~nUsers/1000) %>%
  layer_points(size := 15, fill = ~dayOfWeek) %>% 
  layer_model_predictions(model='lm') %>% 
  scale_ordinal('fill', range=palette) %>%
  add_title(title = 'Users Over Time', x_lab = 'Time')

usersOverTime.vis.NO <- 
  ggvis(usersOverTime[nUsers < quantile(nUsers, .9)], ~Day, ~nUsers/1000) %>%
  layer_points(size := 15, fill = ~dayOfWeek) %>% 
  layer_model_predictions(model='lm') %>% 
  layer_smooths(se=T) %>%
  scale_ordinal('fill', range=palette) %>%
  add_title(title = 'Users Over Time - No Outliers', x_lab = 'Time')


####Total Activity over Time
activityOverTime <- 
  usersOverTime[nActions < quantile(nUsers, .9)] %>%
  ggvis(~Day, ~nActions/1000) %>%
  layer_points(size := 15, fill = ~dayOfWeek) %>%
  group_by(dayOfWeek) %>%
  layer_model_predictions(model='lm') %>% 
  layer_smooths(se=T) %>%
  scale_ordinal('fill', range=palette) %>%
  add_title(title = 'Actions Over Time', x_lab = 'Time')

actionTypesTime <- 
  ds[
    ,
    .(nActionType = .N)
    ,
    by = .(Day, action.type)
  ][,
    actionPct := nActionType / sum(nActionType)
    ,
    by = Day
    ][
    ,
    dayOfWeek := factor(weekdays(Day), levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  ]

actionTypesTime.vis <- 
  ggplot(actionTypesTime, aes(Day, nActionType/1000)) +
  geom_point(size=.9, aes(color=dayOfWeek)) + 
  scale_color_manual(values=palette) +
  geom_smooth(size=.6) + 
  ggtitle('Action Types over Time') + scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") + 
  facet_wrap(~action.type, scales="free_y") + theme(plot.title = element_text(hjust = 0.5))

###User churn
userMonth <- 
  ds[!duplicated(userMonth)]

userMonth[
  ,
  nthVisit := 1:.N
  ,
  by = Uid
][
  ,
  firstVisit := ifelse(nthVisit > 1, F, T)
]

churn <- 
  userMonth %>%
  group_by(monthYear) %>%
  summarise(
    newUsers = length(which(firstVisit==T)) / n(),
    oldUsers = length(which(firstVisit==F)) / n()
  ) %>% 
  # filter(monthYear != '2016-10') %>%
  gather(Type, Percentage, newUsers:oldUsers)

customerChurn <- 
  ggplot(churn, aes(x = monthYear, y=Percentage, group = Type)) + 
  geom_bar(stat='identity', position='stack', width=.5, aes(fill=Type)) + 
  ggtitle('Churn Over Months') + 
  theme(plot.title = element_text(hjust = 0.5))

actionDistribution <- 
  ggplot(actionTypesTime, aes(Day, actionPct)) + 
  geom_bar(width=1, stat='identity', position='stack', aes(fill=action.type)) + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_x_date(date_breaks = "1 month") + labs(title='Action Distribution Over Time', x='Time') + 
  theme(plot.title = element_text(hjust = 0.5))
