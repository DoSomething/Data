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
    count(A, !!pivot) %>%
    group_by(!!pivot) %>%
    mutate(pct=n/sum(n)) %>%
    arrange(-n)

  p1 <-
    ggplot(causeTypes, aes(x=A, y=n, fill=!!pivot)) +
    geom_bar(stat='identity', position='dodge', width = .75) +
    scale_fill_brewer(palette='Set2') +
    labs(x='') +
    theme(axis.text.x = element_text(angle=30, hjust=1))

  p2 <-
    ggplot(causeTypes, aes(x=A, y=pct, fill=!!pivot)) +
    geom_bar(stat='identity', position='dodge', width = .75) +
    scale_fill_brewer(palette='Set2') +
    labs(x='') +
    theme(axis.text.x = element_text(angle=30, hjust=1))

  return(list(p1,p2))

}

getPivotBreakdown(qres,region)

getPivotBreakdown(qres, political_view)

sexDat <-
  qres %>%
  filter(sex %in% c('Male','Female'))
getPivotBreakdown(sexDat, sex)

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

