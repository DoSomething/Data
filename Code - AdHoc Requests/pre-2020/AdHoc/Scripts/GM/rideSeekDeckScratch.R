
# Seatbelt Actions --------------------------------------------------------

outcomes <- c('seatbelt_driver','seatbelt_front','seatbelt_back')
getProportions <- function(dat, outcome, expid=NULL) {
  
  split <- strsplit(outcome, '_')[[1]]
  proportions <-
    dat %>% 
    group_by_(expid, outcome) %>% 
    summarise(count = n()) %>% 
    filter(!is.na(get(outcome))) %>% 
    mutate(Proportion = round(count/sum(count), 3),
           outcome = as.factor(round(get(outcome), 3)))
}
proportions <- tibble()
for (i in 1:length(outcomes)) {
  t <- getProportions(dat, outcomes[i], expid='group')
  proportions <- bind_rows(proportions, t)
}

g <- 
  proportions %>% 
  mutate(
    Seat = 
      ifelse(!is.na(seatbelt_driver), 'Driver',
             ifelse(!is.na(seatbelt_front), 'Front Seat','Rear Seat'))
  ) %>% 
  select(Seat, group, Proportion, outcome) %>% 
  melt(id.var=c('Seat','group','outcome'), 
       value.var=c('Proportion'), value.name='Proportion') 

labels = c('Never/Almost Never', 'Less than Half', 'Most Times', 'Always')
p <- 
  ggplot(g, aes(x=outcome, y=Proportion, fill=group)) + 
  geom_bar(stat='identity', position='dodge', width=.9) +
  labs(x='Proportion', y='',title = '') +
  scale_x_discrete(labels=c(labels)) + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        strip.text.x = element_text(size = 18)) + 
  facet_wrap(~Seat) + scale_fill_manual(values=c('#000000',"#23b7fbff"))
ggsave('Visuals/RnS/seatbeltProportion.png', plot = p,
      width = 19.20,height = 10.80, limitsize = F)

# Other Actions -----------------------------------------------------------
outcomes <- c('distraction_sums','considers_dangers','willing_intervene')

proportions <- tibble()
for (i in 1:length(outcomes)) {
  t <- getProportions(dat, outcomes[i], expid='group')
  proportions <- bind_rows(proportions, t)
}

g <- 
  proportions %>% 
  mutate(
    Seat = 
      ifelse(!is.na(distraction_sums), 'Distractions',
             ifelse(!is.na(considers_dangers), 'Dangers','Intervention')),
    outcome = as.factor(as.numeric(outcome))
  ) %>% 
  select(Seat, group, Proportion, outcome) %>% 
  filter(outcome!=12) %>% 
  melt(id.var=c('Seat','group','outcome'), 
       value.var=c('Proportion'), value.name='Proportion')

p <- 
  ggplot(filter(g, Seat!='Intervention'), 
         aes(x=outcome, y=Proportion, fill=group)) + 
  geom_bar(stat='identity', position='dodge', width=.9) +
  labs(x='', y='',title = 'Risk Acknowledgement') +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold", size=13),
        axis.text.y = element_text(face="bold", size=13),
        strip.text.x = element_text(size = 18)) + 
  facet_wrap(~Seat, scales='free') + 
  scale_fill_manual(values=c('#000000',"#23b7fbff"))
ggsave('Visuals/RnS/riskAssessment.png', plot = p,
       width = 19.20,height = 10.80, limitsize = F)

p <- 
  ggplot(filter(g, Seat=='Intervention'), 
         aes(x=outcome, y=Proportion, fill=group)) + 
  geom_bar(stat='identity', position='dodge', width=.9) +
  labs(x='', y='',title = 'Strength of Intervention') +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(face="bold", size=13),
        axis.text.x = element_text(face="bold", size=13)) + 
  # facet_wrap(~Seat, scales='free') + 
  scale_fill_manual(values=c('#000000',"#23b7fbff"))
ggsave('Visuals/RnS/strengthIntervene.png', plot = p,
       width = 19.20,height = 10.80, limitsize = F)

# Total Impact ------------------------------------------------------------

p <- pivotBars(expid='group')
ggsave('Visuals/RnS/TotalImpact.png', plot = p,
       width = 19.20,height = 10.80, limitsize = F)


# Pivots ------------------------------------------------------------------
controls <- c('region','vehicle')
titles <- c('Region', 'Vehicle Type')
for (i in 1:length(controls)) {
  pivotBars(controls[i], titles[i],expid = 'group') %>% 
    ggsave(paste0('Visuals/RnS/',titles[i],'.png'), plot = .,
           width = 19.20,height = 10.80, limitsize = F)
}

# Age ---------------------------------------------------------------------

p <- 
  dat %>% 
  group_by(group, age) %>% 
  summarise(
    considerDistraction = mean(distraction_prone),
    considerDangers = mean(considers_dangers),
    willingIntervene = mean(willing_intervene),
    wearSeatbelt.Drive = mean(seatbelt_driver, na.rm=T),
    wearSeatbelt.Front = mean(seatbelt_front, na.rm=T),
    wearSeatbelt.Back = mean(seatbelt_back, na.rm=T)
  ) %>%
  melt(id.var=c('group','age')) %>%
  ggplot(.,aes(x=age, y=value, colour=group)) +
  geom_point() + 
  geom_smooth(method='lm', linetype='dotted', size=.8, se=F) +
  geom_line() +
  scale_x_continuous(breaks=pretty_breaks(8)) +
  facet_wrap(~variable) + 
  labs(title='Age',x='') + 
  theme(legend.title=element_blank(),
        axis.text.x = element_text(face="bold", size=13),
        strip.text.x = element_text(size = 18),
        axis.text.y = element_text(face="bold", size=13)) + 
  scale_color_manual(values=c('#000000',"#23b7fbff"))
ggsave('Visuals/RnS/Age.png', plot = p,
       width = 19.20,height = 10.80, limitsize = F)
