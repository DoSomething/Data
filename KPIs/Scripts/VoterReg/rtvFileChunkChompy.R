badEmails <-
  c(
    'testing123@email.com',
    'testing1234@email.com',
    'testing12349@email.com',
    'peanuts123@gmail.com',
    'kitty666@gmail.com',
    'testing123@gmail.com',
    'Shay@gmail.com',
    'mariusandeponime@gmail.com',
    'spark@dosomething.org',
    'puppetsloth111@email.com',
    'lkpttn@gmail.com'
  )

rawRTV <-
  read_csv('Data/RockTheVote/rock_the_vote_2018-09-04.csv') %>%
  filter(
    `Email address` != badEmails
  ) %>%
  mutate(
    `Tracking Source` = gsub('user\\:\\{userid\\}\\,','',`Tracking Source`)
  )


allSets <- split(rawRTV, as.numeric(rownames(rawRTV)) %/% 1200)

for (i in 1:length(allSets)) {

  write_csv(
    allSets[[i]],
    path =
      paste0(
        'Data/RTVChompy/rtv_',
        names(allSets[i]),
        '.csv'
      )
  )

}