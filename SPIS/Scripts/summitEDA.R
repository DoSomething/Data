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

volunFrame <-
  tibble(formal=c(22.5), informal=)