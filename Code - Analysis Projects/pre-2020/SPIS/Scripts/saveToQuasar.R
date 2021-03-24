source('config/pgConnect.R')

channel <- pgConnect()

mem <-
  read_csv('Data/spis_members_raw_values.csv') %>%
  mutate_if(
    is.character, removeSpecialCharacters
  )
gp <-
  read_csv('Data/spis_genpop_raw_values.csv') %>%
  mutate_if(
    is.character, removeSpecialCharacters
  )

channel <- pgConnect()

dbRemoveTable(channel, c("survey","spis_2018"))
dbRemoveTable(channel, c("survey","spis_2018_members"))
dbRemoveTable(channel, c("survey","spis_2018_genpop"))
dbRemoveTable(channel, c("survey","spis_2018"))
dbWriteTable(channel,c("survey", "spis_2018"), set, row.names=F)
dbWriteTable(channel,c("survey", "spis_2018_genpop"), gp, row.names=F)
dbWriteTable(channel,c("survey", "spis_2018_members"), mem, row.names=F)
