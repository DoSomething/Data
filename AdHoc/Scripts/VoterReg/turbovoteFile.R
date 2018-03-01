vr <- 
  read_csv('Data/testing-dosomething.turbovote.org-dosomething.turbovote.org-2018-02-23.csv') %>% 
  filter(
    !grepl('thing.org', email) & 
      !grepl('testing', hostname) & 
      !is.na(`referral-code`)
    )
