smsout <-
  read_csv('Data/smsVoterRegTestOngoing.csv')

smsIn <-
  read_csv('Data/smstest_20180524131641.csv')

combine <-
  smsIn %>%
  left_join(smsout, by = c('nsid' = 'userId'))
