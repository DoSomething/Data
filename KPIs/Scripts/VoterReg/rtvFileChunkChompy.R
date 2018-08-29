rawRTV <- read_csv('Data/RockTheVote/rock_the_vote_2018-08-29.csv')

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