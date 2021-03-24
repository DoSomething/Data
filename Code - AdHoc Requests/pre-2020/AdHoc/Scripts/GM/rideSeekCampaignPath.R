library(scales)

rs <-
  read_csv('Data/rideandseek_moco_signup_export.csv') %>% 
  mutate(
    Scenario1 = case_when(
      ride1=='blast' ~ 'Music',
      ride1=='hit' ~ 'Knowledge'
    ),
    Scenario2 = case_when(
      ride2=='kanye' ~ 'Music',
      ride2=='spout' ~ 'Knowledge',
      TRUE ~ 'Drop Off'
    )
  )

count1 <-
  rs %>% 
  filter(!is.na(ride1)) %>% 
  count(Scenario1) %>% 
  mutate(
    Proportion = n / sum(n),
    Choice = Scenario1,
    Scenario = 'Strategy as Driver'
  ) %>% select(-n,-Scenario1)
count2 <-
  rs %>% 
  filter(!is.na(ride1) & Scenario2 != 'Drop Off') %>% 
  count(Scenario2) %>% 
  mutate(
    Proportion = n / sum(n),
    Choice = Scenario2,
    Scenario = 'Strategy as Fellow Passenger'
  ) %>% select(-n,-Scenario2) 
count <- rbind(count1, count2)


p <- ggplot(count, aes(x=as.factor(Scenario), y=Proportion, fill=Choice)) +
  geom_bar(stat='identity', position = 'dodge') +
  labs(x='Scenario') + scale_fill_manual(values=c("#23b7fbff",'#000000')) +
  theme(
    legend.title=element_blank(),
    legend.text=element_text(size=16),
    axis.text.x = element_text(face="bold", size=16),
    axis.text.y = element_text(face="bold", size=16))
ggsave('Visuals/RnS/interventionStrategy.png', plot = p,
       width = 19.20,height = 10.80, limitsize = F)
