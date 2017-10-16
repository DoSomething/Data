source('config/init.R')
source('config/mySQLConfig.R')
library(broom)

read <- 
  read_csv('Data/mel_2017-10-02_2016-10-01.csv') %>% 
  setNames(c('email','northstar_id','action_type','event_id','ts','northstar_created')) %>% 
  mutate(event_id = seq(1,nrow(.), 1))

mel <-
  read %>% 
  mutate(
    daysSinceCreated = ts - northstar_created
  ) %>% 
  filter(
    ts > '1970-01-01'
  )

ggplot(filter(mel, daysSinceCreated>=0), aes(x=daysSinceCreated)) + geom_density()

northstarCount <- 
  mel %>% 
  filter(daysSinceCreated > 0) %>% 
  summarise(length(unique(northstar_id))) %>% 
  as.numeric()
  
actionRates <-
  mel %>% 
  filter(daysSinceCreated > 0) %>%
  group_by(daysSinceCreated) %>% 
  summarise(
    percentActive = n() / northstarCount
  ) %>% 
  mutate(
    previousRate = lag(percentActive)
  )

actionMod <- 
  lm(log(percentActive) ~ daysSinceCreated + previousRate, data=actionRates)
glance(actionMod)

actionRates$predictActive <- exp(predict(actionMod, actionRates, type="response"))

ggplot(actionRates, aes(x=daysSinceCreated)) + 
  geom_line(aes(y=percentActive)) + 
  geom_line(aes(y=predictActive, color='red')) + 
  labs(
    x='Days Since Registration', 
    y='Percent of Users Active', 
    title='Predicted & Actual User Activity over Lifetime'
    ) +
  theme(plot.title=element_text(hjust = 0.5), legend.position='none') + 
  scale_x_continuous(breaks=pretty_breaks(40))
                                              