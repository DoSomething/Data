smsout <-
  read_csv('Data/smsVoterRegTestOngoing.csv')

smsIn <-
  read_csv('Data/smstest_20180524131641.csv')

combine <-
  smsIn %>%
  inner_join(smsout, by = c('nsid' = 'userId'))
