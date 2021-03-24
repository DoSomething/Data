MoM.jul <-
  MoM %>%
  filter(month==7)

jmod <- lm(value ~ dayOfMonth, MoM.jul)

addDat <- tibble(dayOfMonth=seq(11,31,1), month=as.factor('7'),variable='registerToDate')

MoM.jul %<>%
  bind_rows(addDat)

MoM.jul$p.value = round(predict(jmod, MoM.jul, type='response'))

mons <-
  vr %>%
  group_by(month) %>%
  filter(month != 9) %>%
  summarise(
    registrations=length(which(grepl('register',ds_vr_status)))
  ) %>%
  mutate(preg = ifelse(month<7,registrations,max(MoM.jul$p.value)))

ggplot(mons, aes(x=month, y=registrations)) +
  geom_bar(stat='identity',fill='skyblue4') +
  geom_bar(aes(y=preg), stat='identity', alpha=.5,fill='skyblue4') +
  stat_summary(fun.y = sum, aes(label = ..y..), geom = "text", vjust=-.2, size=6) +
  geom_text(aes(x=7,y=max(preg),label=max(preg)), vjust=-.2, size=6) +
  labs(title='Voter Registrations Per Month',y='',x='') +
  theme_minimal() +
  theme(
    plot.title=element_text(hjust=.5,size=24),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size=16)
    ) +
  scale_fill_brewer(palette = 'Set2') +
  scale_x_continuous(
    breaks=c(1,2,3,4,5,6,7),
    labels=c("January",'February','March','April','May','June','July')
    ) +
  scale_y_continuous(breaks=pretty_breaks(10))
