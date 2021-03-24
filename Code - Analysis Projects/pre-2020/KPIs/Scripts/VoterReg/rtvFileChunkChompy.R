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
  read_csv('Data/RockTheVote/rock_the_vote_2018-10-17.csv') %>%
  filter(
    `Email address` != badEmails
  ) %>%
  mutate(
    `Tracking Source` = gsub('user\\:\\{userid\\}\\,','',`Tracking Source`)
  )


allSets <- split(rawRTV, as.numeric(rownames(rawRTV)) %/% 45000)

for (i in 1:length(allSets)) {

  write_csv(
    allSets[[i]],
    na = '',
    path =
      paste0(
        'Data/RTVChompy/rtv_',
        names(allSets[i]),
        '.csv'
      )
  )

}

prodTest <- sample_n(allSets[[2]],100)
write_csv(prodTest, na = '',
         path =
           paste0(
             'Data/RTVChompy/rtv_prodTest.csv'
           ))
