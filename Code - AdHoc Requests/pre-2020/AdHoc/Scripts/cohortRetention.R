library(tidyverse)
library(data.table)
library(dtplyr)
library(lubridate)
source('Scripts/DSUtils.R')

df <- tbl_dt(read_csv('Data/members_created_dec_01_2016_non_niche.csv'))
names(df) <- c('Uid','event_date','event_id','action_type','action_id','event_ts','user_create_date')

df %<>%
  mutate(
    create_action_delta = as.numeric(event_date - user_create_date),
    create_action_delta_p1 = as.numeric(event_date - user_create_date) + 1,
    sms_outlier = ifelse(action_type=='sms_game' & create_action_delta < quantile(create_action_delta, .00001), 1, 0),
    created_month = month(user_create_date),
    create_action_bucket = factor(
      ifelse(inInterval(create_action_delta, c(0,30)), '0-30',
             ifelse(inInterval(create_action_delta, c(31,60)), '31-60',
                    ifelse(inInterval(create_action_delta, c(61,90)), '61-90',
                           ifelse(inInterval(create_action_delta, c(91,120)), '91-120', 
                                  ifelse(create_action_delta > 120, '120+',
                                         ifelse(create_action_delta < 0, 'Less Than 0', NA)
                                  )
                           )
                    )
             )
      ),
      levels = c('Less Than 0', '0-30', '31-60', '61-90', '91-120', '120+')
    )
  )

df %>% filter(create_action_delta < 0) %>% write_csv(path='~/Desktop/actions_before_create.csv')

ggplot(df[sms_outlier==0], aes(x=action_type, y=log(create_action_delta_p1))) + 
  geom_violin() + 
  coord_flip()

ggplot(df[action_type!='sms_game'], aes(x=action_type, y=create_action_delta)) + geom_violin() + coord_flip()

ggsave(
  filename='Visuals/SMS_Action_User_Create.png',
  ggplot(df[action_type=='sms_game' & create_action_delta > -800], aes(x=create_action_delta)) + 
    geom_density() + labs(title='SMS Action Taken Relative To Created Date')
)

actionFreq <-
  df %>%
  group_by(Uid, created_month, create_action_bucket) %>%
  summarise(
    count = n()
  )

possibleActions <-
  tbl_dt(
    expand.grid(
      Uid = unique(df$Uid), 
      create_action_bucket = unique(df$create_action_bucket)
    )
  )

actions <- 
  possibleActions %>%
  left_join(actionFreq) %>%
  mutate(
    count = ifelse(is.na(count), 0, count)
  )

actions[
  ,
  created_month := min(created_month, na.rm=T),
  by = Uid
][
  ,
  actionTaken := ifelse(count > 0, 1, 0)
]

actionCollapse <- 
  actions[
    ,
    .(pctActionTaken = mean(actionTaken))
    ,
    by = .(create_action_bucket, created_month)
  ]

df[,.N/nrow(df),by=action_type]


