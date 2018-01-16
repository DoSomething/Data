nps <-
  data.frame(
    segment = c('unact-niche', 'act-niche', 'sms', 'others'),
    bsegment = c('other','other','sms','other'),
    current_score = c(-33, -16, 12, 17),
    size = c(84,106,353,253),
    proposed_size = c(25, 79, 400, 300)
  )

nps$proposed_score <- c(-30, 0, 16, 20)
weighted.mean(nps$proposed_score, nps$size)
weighted.mean(nps$proposed_score, nps$proposed_size)

col <-
  nps %>%
  group_by(bsegment) %>%
  summarise(
    score = mean(current_score),
    sample = sum(size)
    ) %>%
  mutate(population = c(other_members, sms_members))
