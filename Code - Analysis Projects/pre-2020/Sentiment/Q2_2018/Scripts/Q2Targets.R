nps <-
  data.frame(
    segment = c('unact-niche', 'act-niche', 'sms', 'others'),
    q4_score = c(-33, -16, 12, 17),
    q4_size = c(84,106,353,253),
    q1_size.target = c(25, 79, 400, 300),
    q1_score.target = c(-30, 0, 16, 20),
    q1_score = c(-7, -3, 14, 6),
    q1_size = c(60, 96, 212, 325),
    q2_score.target = c(0, 5, 16, 20),
    q2_size.target = c(72, 101, 282, 289)
  )

weighted.mean(nps$q2_score.target, nps$q2_size.target)
weighted.mean(nps$q1_score, nps$q1_size)
