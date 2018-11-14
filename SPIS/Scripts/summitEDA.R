source('config/init.R')
library(rlang)
library(glue)

nsids <- set %>% filter(!is.na(External_Reference)) %>% select(External_Reference)

q <-
  glue_sql(
    "SELECT DISTINCT
      c.northstar_id,
      u.zipcode,
      c.signup_id,
      i.campaign_run_id,
      i.campaign_node_id_title,
      i.campaign_cta,
      i.campaign_cause_type
    FROM campaign_activity c
    LEFT JOIN users u
      ON u.northstar_id = c.northstar_id
    LEFT JOIN campaign_info i
      ON i.campaign_run_id = c.campaign_run_id
    WHERE c.northstar_id IN ({nsids*})",
    .con = pg,
    nsids = nsids$External_Reference
  )

qres <-
  runQuery(q) %>%
  left_join(
    set %>%
      select(External_Reference, age, race, sex, zipcode, fam_finances,
             grade_level, region, political_view),
    by = c('northstar_id' = 'External_Reference')
  )

qres %>%
  count(campaign_node_id_title) %>% arrange(-n)

getPivotBreakdown <- function(dat,pivot) {

  pivot <- enquo(pivot)

  temp <-
    dat %>%
    separate(campaign_cause_type, into=LETTERS[1:4], remove=F, sep = ',')

  causeTypes <-
    bind_rows(
      temp %>% filter(!is.na(A) & A!='') %>% select(A, !!pivot),
      temp %>% filter(!is.na(B) & B!='') %>% select(A=B, !!pivot),
      temp %>% filter(!is.na(C) & C!='') %>% select(A=C, !!pivot),
      temp %>% filter(!is.na(D) & D!='') %>% select(A=D, !!pivot)
    ) %>%
    mutate(A=gsub(' ','',A)) %>%
    mutate(
      A =
        case_when(
          A %in% c('MentalHealth','Bullying') ~ 'Mental Health / Bullying',
          A %in% c('Homelessness','Poverty') ~ 'Poverty/Homelessness',
          TRUE ~ A
        )
    ) %>%
    filter(!A %in% c('Animals','Disasters','PhysicalHealth','Relationships','Sex')) %>%
    count(A, !!pivot) %>%
    group_by(!!pivot) %>%
    mutate(pct=n/sum(n)) %>%
    arrange(-n)

  # p1 <-
  #   ggplot(causeTypes, aes(x=A, y=n, fill=!!pivot)) +
  #   geom_bar(stat='identity', position='dodge', width = .75) +
  #   scale_fill_brewer(palette='Set2') +
  #   labs(x='') +
  #   facet_wrap(~A) +
  #   theme(axis.text.x = element_text(angle=30, hjust=1))

  p2 <-
    ggplot(causeTypes, aes(x=A, y=pct, fill=!!pivot)) +
    geom_bar(stat='identity', position='dodge', width = .75) +
    scale_fill_brewer(palette='Set2') +
    labs(x='', y='') +
    facet_wrap(~A, scales='free')

  return(p2)

}

region <-
  getPivotBreakdown(qres,region) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),
    strip.text.x=element_text(size=12)) +
  guides(fill=guide_legend(title="Region"))

getPivotBreakdown(qres, political_view)

sexDat <-
  qres %>%
  filter(sex %in% c('Male','Female'))
sexPlot <-
  getPivotBreakdown(sexDat, sex) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),
    strip.text.x=element_text(size=12)) +
  guides(fill=guide_legend(title="Gender"))

gradeDat <-
  qres %>%
  mutate(
    grade_level =
      case_when(
        grepl('2', grade_level) ~ '2 Year College',
        grepl('4', grade_level) ~ '4 Year College',
        grade_level=='High School' ~ 'High School',
        TRUE ~ NA_character_
      )
  ) %>%
  filter(
    grade_level %in% c('High School','2 Year College','4 Year College')
  )
getPivotBreakdown(gradeDat, grade_level)


poliAlign <-
  agreePosition.Politics %>%
  filter(
      Group=='Gen Pop' &
      political_view %in% c('Moderate','Liberal','Conservative')
    )

racePlot <-
  getPivotBreakdown(qres %>% filter(race %in% c('Black','White','Hispanic/Latino')), race) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),
    strip.text.x=element_text(size=12)) +
  guides(fill=guide_legend(title="Race"))

financePlot <-
  getPivotBreakdown(qres %>% filter(fam_finances != "Don't Know"), fam_finances) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_text(size=12),
    strip.text.x=element_text(size=12)) +
  guides(fill=guide_legend(title="Finances"))


ggplot(filter(poliAlign,grepl('Climate',quest)), aes(x=political_view,y=avgVal)) +
  geom_bar(stat='identity') +
  labs(x='',y='',
    title='Climate Change is Happening and is Caused by Human Activity') +
  scale_y_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree'))

ggplot(filter(poliAlign,grepl('Guns',quest)), aes(x=political_view,y=avgVal)) +
  geom_bar(stat='identity') +
  labs(x='',y='',
       title='Background Checks Should be Required for all Gun Purchases') +
  scale_y_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree'))

ggplot(filter(poliAlign,grepl('Health',quest)), aes(x=political_view,y=avgVal)) +
  geom_bar(stat='identity') +
  labs(x='',y='',
       title='The Government has the Responsibility to Ensure Health Coverage for All') +
  scale_y_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree'))

ggplot(
  filter(poliAlign,grepl('Muslims',quest) | grepl('Immigrants',quest)),
  aes(x=political_view,y=avgVal)
  ) +
  geom_bar(stat='identity', position='dodge') +
  labs(x='',y='') +
  scale_y_continuous(breaks=seq(-2,2,1), limits = c(-2,2),
                     labels =c('Strongly Disagree','Disagree','Neutral',
                               'Agree','Strongly Agree')) +
  facet_wrap(~quest)

ggplot(volunteerReason$overall$data %>%
         filter(!variable %in% c('Because_my_family_does_it','Because_I_want_to_feel_connected_to_my_community')) %>%
         mutate(variable = gsub('_',' ',variable),
                variable = gsub('Because ','',variable),
                variable = gsub('it','It',variable),
                variable = gsub('my','My',variable)),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', fill='#2fe4daff') +
  geom_label(aes(label=percent(value)),vjust=-.1,size=6) +
  labs(x='',title='',y='') +
  scale_y_continuous(minor_breaks = seq(0,.8,.1),breaks=seq(0,.8,.1), limits = c(0,.8)) +
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(size=14, color='white'),
        axis.text.y = element_text(color='white'),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#332baaff", colour = "#332baaff"),
        panel.grid.major.y = element_line(color='white'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#332baaff"))


testTheme <-
  ggplot(volunteerReason$overall$data %>%
         filter(!variable %in% c('Because_my_family_does_it','Because_I_want_to_feel_connected_to_my_community')) %>%
         mutate(variable = gsub('_',' ',variable),
                variable = gsub('Because ','',variable),
                variable = gsub('it','It',variable),
                variable = gsub('my','My',variable)),
       aes(x=reorder(variable, -value), y=value)) +
  geom_bar(stat='identity', fill='#2fe4daff') +
  geom_label(aes(label=percent(value)),vjust=-.1,size=6) +
  labs(x='',title='',y='') +
  scale_y_continuous(minor_breaks = seq(0,.8,.1),breaks=seq(0,.8,.1), limits = c(0,.8))

dsTheme(testTheme)

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_ != 'Unfamiliar with this topic') %>%
  mutate(
    cchange=
      ifelse(agree_positions.Climate_change_is_happening_and_is_caused_by_human_activity_ %in%
               c('Agree','Strongly Agree'),1,0)) %>%
  group_by(political_view) %>%
  summarise(mean(cchange))
climateChange.d <-
  tibble(
    People = c('Old People','Old People','Young People','Young People'),
    Partisanship = c('Conservative','Democrat','Conservative','Democrat'),
    `% Agree` = c(.15,.79,.62,.88)
    )
dsTheme(
  ggplot(climateChange.d,aes(x=People, y=`% Agree`,fill=Partisanship)) +
  geom_bar(stat='identity', position='dodge') +
  geom_label(aes(label=percent(`% Agree`)),vjust=-.1,size=6,position=position_dodge(width = .9)) +
  labs(x='',title='',y='') +
  scale_y_continuous(minor_breaks = seq(0,.9,.1),breaks=seq(0,.9,.1), limits = c(0,.9)) +
  guides(fill=FALSE)
)

set %>%
  filter(Group=='Gen Pop' &
           agree_positions.Racism_is_no_longer_an_issue_in_America_ != 'Unfamiliar with this topic') %>%
  mutate(
    cchange=
      ifelse(agree_positions.Racism_is_no_longer_an_issue_in_America_ %in%
               c('Agree','Strongly Agree'),1,0)) %>%
  group_by(political_view) %>%
  summarise(mean(cchange))
race.d <-
  tibble(
    People = c('Old People','Old People','Young People','Young People'),
    Partisanship = c('Conservative','Democrat','Conservative','Democrat'),
    `% Agree` = c(.36,.81,.75,.89)
  )
dsTheme(
  ggplot(race.d,aes(x=People, y=`% Agree`,fill=Partisanship)) +
    geom_bar(stat='identity', position='dodge') +
    geom_label(aes(label=percent(`% Agree`)),vjust=-.1,size=6,position=position_dodge(width = .9)) +
    labs(x='',title='',y='') +
    scale_y_continuous(minor_breaks = seq(0,.9,.1),breaks=seq(0,.9,.1), limits = c(0,.92)) +
    guides(fill=FALSE)
)

set %>%
  filter(Group=='Gen Pop' &What
           agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all != 'Unfamiliar with this topic') %>%
  mutate(
    cchange=
      ifelse(agree_positions.The_government_has_the_responsibility_to_ensure_health_coverage_for_all %in%
               c('Agree','Strongly Agree'),1,0)) %>%
  group_by(political_view) %>%
  summarise(mean(cchange))
healthcare.d <-
  tibble(
    People = c('Old People','Old People','Young People','Young People'),
    Partisanship = c('Conservative','Democrat','Conservative','Democrat'),
    `% Agree` = c(.24,.77,.58,.84)
  )
dsTheme(
  ggplot(healthcare.d,aes(x=People, y=`% Agree`,fill=Partisanship)) +
    geom_bar(stat='identity', position='dodge') +
    geom_label(aes(label=percent(`% Agree`)),vjust=-.1,size=6,position=position_dodge(width = .9)) +
    labs(x='',title='',y='') +
    scale_y_continuous(minor_breaks = seq(0,.9,.1),breaks=seq(0,.9,.1), limits = c(0,.9)) +
    guides(fill=FALSE)
)
